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
  , weakComponents
  , strictComponents
  , allComponents
  , dpComponents
  , trsComponents
  , isDPProblem
  , mapRules
  , measureName
  , pprintComponents
  , wellFormed)
where

import Data.Set (Set)

import qualified Termlib.Trs as Trs
import Termlib.Trs.PrettyPrint (pprintNamedTrs)
import Termlib.Trs (Trs) 
import Termlib.Variable (Variables)
import Termlib.FunctionSymbol (Signature, Symbol)
import Termlib.ContextSensitive (ReplacementMap)
import Termlib.Utils
import Text.PrettyPrint.HughesPJ

data Strategy = Innermost
              | Full 
              | ContextSensitive ReplacementMap
              | Outermost deriving (Eq, Show)

data StartTerms = BasicTerms {defineds :: Set Symbol
                             , constrs :: Set Symbol}
                | TermAlgebra 
                  deriving (Eq, Show)


data Ruleset = Ruleset { sdp  :: Trs
                       , wdp  :: Trs
                       , strs :: Trs
                       , wtrs :: Trs }

data Problem = Problem { startTerms  :: StartTerms
                       , strategy    :: Strategy
                       , variables   :: Variables
                       , signature   :: Signature
                       , strictDPs   :: Trs 
                       , strictTrs   :: Trs
                       , weakDPs     :: Trs
                       , weakTrs     :: Trs } 
               deriving (Eq)

ruleset :: Problem -> Ruleset
ruleset prob = Ruleset { sdp  = strictDPs prob
                       , wdp  = weakDPs prob
                       , strs = strictTrs prob
                       , wtrs = weakTrs prob }

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

isDPProblem :: Problem -> Bool
isDPProblem prob = not (Trs.isEmpty $ dpComponents prob)
                   && case startTerms prob of BasicTerms{} -> True; _ -> False

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
pprintComponents prob = 
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
          mt (BasicTerms _ _) = text "runtime"
          mt TermAlgebra    = text "derivational"

