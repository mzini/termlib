{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE EmptyDataDecls #-}

module Termlib.Problem
  ( Strategy(..)
  , StartTerms(..)
  , Relation(..)
  , Problem
  , problem
  , standardProblem
  , dpProblem
  , relativeProblem
  , onProblem
  , withStandardProblem
  , withDpProblem
  , withRelativeProblem)
where

import qualified Termlib.Trs as Trs 
import Termlib.Trs (Trs) 
import Termlib.FunctionSymbol (Signature)

data Strategy = Innermost
              | Outermost
              | Full deriving (Show,Eq)

data StartTerms = BasicTerms Signature
                | TermAlgebra 
                  deriving Show

data Relation = Standard Trs 
              | DP Trs Trs
              | Relative Trs Trs 
                deriving Show

data Problem = Problem {startTerms :: StartTerms
                       , strategy :: Strategy
                       , relation :: Relation} 
               deriving Show

problem :: StartTerms -> Strategy -> Relation -> Problem
problem = Problem 

standardProblem :: StartTerms -> Strategy -> Trs -> Problem
standardProblem t s r = Problem t s (Standard r)

dpProblem :: StartTerms -> Strategy -> Trs -> Trs -> Problem
dpProblem t s sr wr = Problem t s (DP sr wr)

relativeProblem :: StartTerms -> Strategy -> Trs -> Trs -> Problem
relativeProblem t s sr wr = Problem t s (Relative sr wr)

onProblem ::  (StartTerms -> Strategy -> Trs -> a) -- ^ called on standard problems
            -> (StartTerms -> Strategy -> Trs -> Trs -> a) -- ^ called on DP problems
            -> (StartTerms -> Strategy -> Trs -> Trs -> a) -- ^ called on relative problems
            -> Problem 
            -> a
onProblem fs fdp frel p = case relation p of 
                            (Standard trs)         -> fs (startTerms p) (strategy p) trs
                            (DP strict weak)       -> fdp (startTerms p) (strategy p) strict weak
                            (Relative strict weak) -> frel (startTerms p) (strategy p) strict weak

withStandardProblem :: (StartTerms -> Strategy -> Trs -> a) -> Problem -> Maybe a
withStandardProblem f = onProblem (\ s r t -> Just $ f s r t) n n
  where n _ _ _ _ = Nothing

withDpProblem :: (StartTerms -> Strategy -> Trs -> Trs -> a) -> Problem -> Maybe a
withDpProblem f = onProblem n (\ s r ts tw -> Just $ f s r ts tw) m
  where n _ _ _   = Nothing
        m _ _ _ _ = Nothing

withRelativeProblem :: (StartTerms -> Strategy -> Trs -> Trs -> a) -> Problem -> Maybe a
withRelativeProblem f = onProblem n m (\ s r ts tw -> Just $ f s r ts tw)
  where n _ _ _   = Nothing
        m _ _ _ _ = Nothing


-- innermostRuntimeComplexity :: Trs -> Problem
-- innermostRuntimeComplexity trs = Problem (BasicTerms $ Trs.definedSymbols trs) Innermost (Standard trs)

-- runtimeComplexity :: Trs -> Problem
-- runtimeComplexity trs = Problem (BasicTerms $ Trs.definedSymbols trs) Full (Standard trs)

-- derivationalComplexity :: Trs -> Problem
-- derivationalComplexity trs = Problem TermAlgebra Full (Standard trs)

-- innermostDpProblem :: Trs -> Trs -> Problem
-- innermostDpProblem strict weak = Problem (BasicTerms $ Trs.definedSymbols strict) Innermost (DP strict weak)

-- dpStrict p = case relation p of 
--                (DP strict _) -> Just strict
--                _             -> Nothing
-- dpWeak p  = case relation p of 
--                (DP _ weak) -> Just weak
--                _           -> Nothing

-- relativeStrict p = case relation p of 
--                      (Relative strict _) -> Just strict
--                      _                   -> Nothing
-- relativeWeak p  = case relation p of 
--                      (Relative _ weak) -> Just weak
--                      _                 -> Nothing

-- trs p = case relation p of 
--           (Standard trs) -> Just trs
--           _              -> Nothing


