{-
This file is part of the Haskell Term Rewriting Library.

The Haskell Term Rewriting Library is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The Haskell Term Rewriting Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the Haskell Term Rewriting Library.  If not, see <http://www.gnu.org/licenses/>.
-}

module Termlib.Problem
  ( Strategy(..)
  , StartTerms(..)
  , Problem(..)
  , Ruleset(..)
  , ReplacementMap
  , ruleset
  , emptyRuleset
  , weakComponents
  , strictComponents
  , allComponents
  , dpComponents
  , trsComponents
  , isDPProblem
  , isRCProblem
  , isDCProblem
  , mapRules
  , measureName
  , pprintComponents
  , withFreshCompounds
  , sanitise
  , wellFormed)
where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Control.Monad (foldM)
import qualified Termlib.Trs as Trs
import Termlib.Trs.PrettyPrint (pprintNamedTrs)
import Termlib.Trs (Trs) 
import Termlib.Variable (Variables)
import Termlib.FunctionSymbol (Signature, Symbol, isMarked)
import Termlib.ContextSensitive (ReplacementMap)
import Termlib.Utils
import qualified Termlib.Term as Term
import qualified Termlib.Rule as R
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import qualified Termlib.Signature as Sig
import Text.PrettyPrint.HughesPJ

data Strategy = Innermost
              | Full 
              | ContextSensitive ReplacementMap
              | Outermost deriving (Eq, Show)

data StartTerms = BasicTerms {defineds :: Set Symbol
                             , constrs :: Set Symbol}
                | TermAlgebra (Set Symbol)
                  deriving (Eq, Show)


data Ruleset = Ruleset { sdp  :: Trs -- ^ strict dependency pairs
                       , wdp  :: Trs -- ^ weak dependency pairs
                       , strs :: Trs -- ^ strict rules
                       , wtrs :: Trs  -- ^ weak rules
                       }

data Problem = Problem { startTerms  :: StartTerms -- ^ considered start-terms
                       , strategy    :: Strategy -- ^ considered strategy
                       , variables   :: Variables -- ^ underlying set of variables
                       , signature   :: Signature -- ^ underlying signature
                       , strictDPs   :: Trs  -- ^ strict dependency pairs
                       , strictTrs   :: Trs -- ^ strict rules
                       , weakDPs     :: Trs -- ^ weak dependency pairs
                       , weakTrs     :: Trs -- ^ weak rules
                       } 
               deriving (Eq, Show)

ruleset :: Problem -> Ruleset
ruleset prob = Ruleset { sdp  = strictDPs prob
                       , wdp  = weakDPs prob
                       , strs = strictTrs prob
                       , wtrs = weakTrs prob }

emptyRuleset :: Ruleset 
emptyRuleset = Ruleset Trs.empty Trs.empty Trs.empty Trs.empty

weakComponents :: Problem -> Trs
weakComponents prob = weakDPs prob `Trs.union` weakTrs prob

strictComponents :: Problem -> Trs
strictComponents prob = strictDPs prob `Trs.union` strictTrs prob

allComponents :: Problem -> Trs
allComponents prob = weakComponents prob `Trs.union` strictComponents prob

dpComponents :: Problem -> Trs
dpComponents prob = strictDPs prob `Trs.union` weakDPs prob

trsComponents :: Problem -> Trs
trsComponents prob = strictTrs prob `Trs.union` weakTrs prob

wellFormed :: Problem -> Bool
wellFormed = Trs.wellFormed . allComponents

isRCProblem :: Problem -> Bool
isRCProblem prob = 
  case startTerms prob of 
    BasicTerms {} -> True
    _             -> False

isDCProblem :: Problem -> Bool
isDCProblem prob = 
  case startTerms prob of 
    TermAlgebra {} -> True
    _              -> False
    
isDPProblem :: Problem -> Bool
isDPProblem prob = 
  case startTerms prob of 
    BasicTerms ds _ -> Foldable.all (isMarked sig) ds  
    _               -> False
  where sig = signature prob
        
mapRules :: (Trs -> Trs) -> Problem -> Problem
mapRules f prob = prob { strictDPs = f $ strictDPs prob
                       , strictTrs = f $ strictTrs prob
                       , weakDPs   = f $ weakDPs prob
                       , weakTrs   = f $ weakTrs prob }

instance PrettyPrintable Problem where 
  pprint prob = pprintComponents prob
                $+$ block "StartTerms" (ppStartTerms (startTerms prob))
                $+$ block "Strategy"   (ppStrategy (strategy prob))
      where ppStartTerms TermAlgebra{} = text "all"
            ppStartTerms BasicTerms{}  = text "basic terms"
            ppStrategy ContextSensitive{} = text "context sensitive"
            ppStrategy Full{}             = text "none"
            ppStrategy Innermost{}        = text "innermost"
            ppStrategy Outermost{}        = text "outermost"

pprintComponents :: Problem -> Doc
pprintComponents prob 
  | null (Trs.toRules $ allComponents prob) = block "Rules" (text "Empty")
  | otherwise =
    ppTrs "Strict DPs" (strictDPs prob)
    $+$ ppTrs "Strict Trs" (strictTrs prob)
    $+$ ppTrs "Weak DPs"   (weakDPs prob)
    $+$ ppTrs "Weak Trs"   (weakTrs prob)
    where sig           = signature prob
          vars          = variables prob
          ppTrs = pprintNamedTrs sig vars
           
measureName :: Problem -> Doc
measureName p = ms (strategy p) <+> mt (startTerms p) <> text "-complexity"
    where ms Innermost = text "innermost"
          ms Outermost = text "outermost"
          ms (ContextSensitive _) = text "context-sensitive"
          ms Full      = empty
          mt BasicTerms {} = text "runtime"
          mt TermAlgebra {}    = text "derivational"

sanitise :: Problem -> Problem
sanitise prob = prob { signature = signature prob `Sig.restrictToSymbols` syms
                     , variables = variables prob `Sig.restrictToSymbols` vars }
  where rs = allComponents prob
        syms = stSyms `Set.union` Trs.functionSymbols rs
        stSyms = case startTerms prob of 
                   BasicTerms ds cs -> ds `Set.union` cs
                   TermAlgebra fs   -> fs
        vars = Set.fromList [ i | V.User i <- Set.toList $ Trs.variables rs]

withFreshCompounds :: Problem -> Problem
withFreshCompounds prob = 
  fst . flip Sig.runSignature (signature prob)  $ do 
    sdps' <- mapM frsh $ zip [1..] sdps 
    wdps' <- mapM frsh $ zip [length sdps + 1..] wdps 
    sig' <- Sig.getSignature
    return $ prob { signature = sig'
                  , strictDPs = Trs.fromRules sdps'
                  , weakDPs = Trs.fromRules wdps'}
   where 
     sdps = Trs.rules $ strictDPs prob
     wdps = Trs.rules $ weakDPs prob
     
     dp i l rs = do
       c <- F.fresh (F.defaultAttribs ("c_" ++ show i) (length rs)) {F.symIsCompound = True}
       return $ R.fromPair (l,Term.Fun c rs)
         
     frsh (i, R.Rule l r@(Term.Var _)) = dp i l [r]
     frsh (i, R.Rule l r@(Term.Fun f rs)) = do 
       attribs <- F.getAttributes f
       if F.symIsCompound attribs then dp i l rs else dp i l [r]

