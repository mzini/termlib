
{-# LANGUAGE BangPatterns #-}

module Termlib.Rewrite where 

import Termlib.Term
import Termlib.Rule 
import Termlib.Substitution
import Termlib.Trs

import Data.Maybe

type Pos = [Int]

data Reduct = Reduct {
     result :: Term,
     pos :: Pos,
     rule :: Rule,
     subst :: Substitution
}

type Strategy = Term -> [Reduct]

fullRewrite :: Trs -> Strategy 
fullRewrite trs t = 
    rootRewrite trs t ++ nested (fullRewrite trs) t

outerRewrite :: Trs -> Strategy 
outerRewrite trs t = 
    case rootRewrite trs t of 
      [] -> nested (outerRewrite trs) t
      rs -> rs

innerRewrite :: Trs -> Strategy
innerRewrite trs t = 
    case nested (rootRewrite trs) t of 
      [] -> rootRewrite trs t
      rs -> rs

rootRewrite :: Trs -> Strategy 
rootRewrite trs t = do 
  r <- toRules trs
  s <- maybeToList $ match (lhs r) t
  t' <- [apply s (rhs r)]
  return Reduct{ result = t', pos = [], rule = r, subst = s }

nested :: Strategy -> Strategy
nested _ (Var _) = []
nested s (Fun f ts) = do
    (n, cl, t) <- listContexts ts
    (\r -> r{ result = Fun f (cl (result r)), pos = n : pos r }) `fmap` s t

-- | Return a list of contexts of a list. Each returned element is an element
-- index (starting from 0), a function that replaces the list element by a
-- new one, and the original element.
listContexts :: [a] -> [(Int, a -> [a], a)]
listContexts = go 0 id where
    go !n f [] = []
    go !n f (x:xs) = (n, f . (: xs), x) : go (n+1) (f . (x:)) xs
