module Termlib.Problem
  ( Strategy(..)
  , StartTerms(..)
  , Relation(..)
  , Problem(..)
  , problem
  , standardProblem
  , dpProblem
  , relativeProblem
  , onProblem
  , strictTrs
  , withStandardProblem
  , withDpProblem
  , withRelativeProblem
  , measureName 
  , prettyPrintRelation)
where

import Data.Set (Set)

import qualified Termlib.Trs as Trs 
import Termlib.Trs.PrettyPrint
import Termlib.Trs (Trs) 
import Termlib.Variable (Variables)
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Signature, Symbol)
import Termlib.Utils 
import Text.PrettyPrint.HughesPJ

data Strategy = Innermost
              | Full deriving (Eq, Show)

data StartTerms = BasicTerms (Set Symbol)
                | TermAlgebra 
                  deriving (Eq, Show)

data Relation = Standard Trs 
              | DP Trs Trs
              | Relative Trs Trs 
                deriving (Eq, Show)

data Problem = Problem {startTerms :: StartTerms
                       , strategy :: Strategy
                       , relation :: Relation
                       , variables :: Variables
                       , signature :: Signature} 
               deriving (Eq, Show)

problem :: StartTerms -> Strategy -> Relation -> Variables -> Signature -> Problem
problem = Problem 

standardProblem :: StartTerms -> Strategy -> Trs -> Variables -> Signature -> Problem
standardProblem t s r = Problem t s (Standard r)

dpProblem :: StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> Problem
dpProblem t s sr wr = Problem t s (DP sr wr)

relativeProblem :: StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> Problem
relativeProblem t s sr wr = Problem t s (Relative sr wr)

onProblem ::  (StartTerms -> Strategy -> Trs -> Variables -> Signature -> a) -- ^ called on standard problems
            -> (StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> a) -- ^ called on DP problems
            -> (StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> a) -- ^ called on relative problems
            -> Problem
            -> a
onProblem fs fdp frel p = case relation p of 
                            (Standard trs)         -> fs (startTerms p) (strategy p) trs (variables p) (signature p)
                            (DP strict weak)       -> fdp (startTerms p) (strategy p) strict weak (variables p) (signature p)
                            (Relative strict weak) -> frel (startTerms p) (strategy p) strict weak (variables p) (signature p)

withStandardProblem :: (StartTerms -> Strategy -> Trs -> Variables -> Signature -> a) -> Problem -> Maybe a
withStandardProblem f = onProblem (\ s r t v sig -> Just $ f s r t v sig) n n
  where n _ _ _ _ _ _ = Nothing

withDpProblem :: (StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> a) -> Problem -> Maybe a
withDpProblem f = onProblem n (\ s r ts tw v sig -> Just $ f s r ts tw v sig) m
  where n _ _ _ _ _  = Nothing
        m _ _ _ _ _ _ = Nothing

withRelativeProblem :: (StartTerms -> Strategy -> Trs -> Trs -> Variables -> Signature -> a) -> Problem -> Maybe a
withRelativeProblem f = onProblem n m (\ s r ts tw v sig -> Just $ f s r ts tw v sig)
  where n _ _ _ _ _ = Nothing
        m _ _ _ _ _ _ = Nothing





instance PrettyPrintable Problem where 
  pprint (Problem terms strategy (Standard trs) vars sig) = text "REWRITE relation according to the following TRS" 
                                                             <+> pphlp_terms terms <+> pphlp_strat strategy $+$ (nest 1 $ pprint (trs, sig, vars))

  pprint (Problem terms strategy (DP strict weak) vars sig) = text "DEPENDENCY PAIR problem according to the following TRSs" 
                                                              <+> pphlp_terms terms <+> pphlp_strat strategy 
                                                              $+$ (nest 1 (text "strict rules:" $+$ pprint (strict, sig, vars)
                                                                           $+$ text "weak rules:" $+$ pprint (weak, sig, vars)))

  pprint (Problem terms strategy (Relative strict weak) vars sig) = text "RELATIVE problem according to the following TRSs" 
                                                                    <+> pphlp_terms terms <+> pphlp_strat strategy 
                                                                    $+$ (nest 1 (text "strict rules:" $+$ pprint (strict, sig, vars)
                                                                                        $+$ text "weak rules:" $+$ pprint (weak, sig, vars)))


pphlp_terms (BasicTerms _) = text "restricted to basic start-terms"
pphlp_terms _ = empty

pphlp_strat Innermost = text "and innermost reductions"
pphlp_strat _ = empty

strictTrs :: Problem -> Trs
strictTrs prob = case relation prob of
                   Standard trs   -> trs
                   DP trs _       -> trs
                   Relative trs _ -> trs



measureName :: Problem -> Doc
measureName p = ms (strategy p) <+> mn (relation p) <+> mt (startTerms p) <> text "-complexity"
    where mn (Standard _)   = empty
          mn (DP _ _)       = text"DP"
          mn (Relative _ _) = text "relative"
          ms Innermost = text "innermost"
          ms Full      = empty
          mt (BasicTerms _) = text "runtime"
          mt TermAlgebra    = text "derivational"



prettyPrintRelation :: Problem -> Doc
prettyPrintRelation p = 
    case relation p of 
      Standard trs         -> ppt "TRS" trs
      DP strict weak       -> ppt "DP" strict $+$ ppt "WEAK" weak
      Relative strict weak -> ppt "STRICT" strict $+$ ppt "WEAK" weak
    where ppt n trs = hang (text $ n ++ ":") 2 $ pprint (trs, signature p, variables p)