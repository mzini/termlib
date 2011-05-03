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
  , ReplacementMap
  , weakRules
  , strictRules
  , allRules
  , dpRules
  , trsRules
  , isDPProblem
  , measureName 
  , wellFormed)
where

import Data.Set (Set)

import qualified Termlib.Trs as Trs
import Termlib.Trs.PrettyPrint()
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

data Problem = Problem { startTerms  :: StartTerms
                       , strategy    :: Strategy
                       , variables   :: Variables
                       , signature   :: Signature
                       , strictDPs   :: Trs 
                       , strictTRS   :: Trs
                       , weakDPs     :: Trs
                       , weakTRS     :: Trs
                       } 
               deriving (Eq, Show)

weakRules :: Problem -> Trs
weakRules prob = weakDPs prob `Trs.union` weakTRS prob

strictRules :: Problem -> Trs
strictRules prob = strictDPs prob `Trs.union` strictTRS prob

allRules :: Problem -> Trs
allRules prob = weakRules prob `Trs.union` strictRules prob

dpRules :: Problem -> Trs
dpRules prob = strictDPs prob `Trs.union` weakDPs prob

trsRules :: Problem -> Trs
trsRules prob = strictTRS prob `Trs.union` weakTRS prob

wellFormed :: Problem -> Bool
wellFormed = Trs.wellFormed . allRules

isDPProblem :: Problem -> Bool
isDPProblem = not . Trs.isEmpty . dpRules

instance PrettyPrintable Problem where 
  pprint prob = ppTrs "Strict DPs" (strictDPs prob)
                $+$ ppTrs "Strict TRS" (strictTRS prob)
                $+$ ppTrs "Weak DPs"   (weakDPs prob)
                $+$ ppTrs "Weak TRS"   (weakTRS prob)
                $+$ block "StartTerms" (ppStartTerms (startTerms prob))
                $+$ block "Strategy"   (ppStrategy (strategy prob))
      where sig           = signature prob
            vars          = variables prob
            block h doc   = hang (text (h ++ ":")) 2 $ doc
            ppTrs n rules = block n $ pprint (rules, sig, vars)
            ppStartTerms TermAlgebra{} = text "all"
            ppStartTerms BasicTerms{}  = text "basic terms"
            ppStrategy ContextSensitive{} = text "context sensitive"
            ppStrategy Full{}             = text "none"
            ppStrategy Innermost{}        = text "innermost"
            ppStrategy Outermost{}        = text "outermost"

measureName :: Problem -> Doc
measureName p = ms (strategy p) <+> mt (startTerms p) <> text "-complexity"
    where ms Innermost = text "innermost"
          ms Outermost = text "outermost"
          ms (ContextSensitive _) = text "context-sensitive"
          ms Full      = empty
          mt (BasicTerms _ _) = text "runtime"
          mt TermAlgebra    = text "derivational"

