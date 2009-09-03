{-# LANGUAGE FlexibleInstances #-}

module Termlib.Rule
  -- (
  -- rewrites,
  -- topRewrites,
  -- reduced,
  -- topReduced,
  -- bothsides,
  -- lhsVariables,
  -- rhsVariables,
  -- isRewriteRule,
  -- duplicating,
  -- nonduplicating,
  -- flat,
  -- shallow,
  -- linear,
  -- ground,
  -- leftFlat,
  -- leftShallow,
  -- leftLinear,
  -- leftGround,
  -- rightFlat,
  -- rightShallow,
  -- rightLinear,
  -- rightGround,
  -- Rule(..)
  -- ) 
where

import qualified Termlib.Substitution as S
import qualified Termlib.Term as T
import Termlib.Term (Term)
import Termlib.Variable (Variable)
import Termlib.FunctionSymbol (Symbol)
import Termlib.Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Maybe as Maybe

data Rule = Rule {lhs :: Term, rhs :: Term} deriving Show

invert :: Rule -> Rule
invert (Rule l r) = Rule r l

fromPair :: (Term, Term) -> Rule
fromPair (l,r) = Rule l r


toPair :: Rule -> (Term, Term)
toPair (Rule l r) = (l,r)

variables :: Rule -> Set Variable
variables (Rule l r) = T.variables l `Set.union` T.variables r

functionSymbols :: Rule -> Set Symbol
functionSymbols (Rule l r) = T.functionSymbols l `Set.union` T.functionSymbols r

instance Eq Rule where
  r1 == r2 = lhs1 == lhs2 && canonrhs vm1 r1 == canonrhs vm2 r2
    where (lhs1, vm1) = T.canonise (lhs r1) Map.empty
          (lhs2, vm2) = T.canonise (lhs r2) Map.empty
          canonrhs vm = fst . (`T.canonise` vm) . rhs

rewrites :: Term -> Term -> Rule -> Bool
rewrites s@(T.Fun f xs) t@(T.Fun g ys) r
  | f == g && length xs == length ys = topRewrites s t r || any (\(x, y) -> rewrites x y r) (zip xs ys)
rewrites s t r = topRewrites s t r

topRewrites :: Term -> Term -> Rule -> Bool
topRewrites s t r = maybe False (Maybe.isJust . S.match t (rhs r)) (S.match s (lhs r) S.empty)

reduced :: Term -> Rule -> Bool
reduced s@(T.Var _) r = topReduced s r
reduced s@(T.Fun _ xs) r = topReduced s r && all (`reduced` r) xs

topReduced :: Term -> Rule -> Bool
topReduced s r = not (lhs r `S.subsumes` s)


bothsides :: (Term -> Bool) -> Rule -> Bool
bothsides f r = (f . lhs) r && (f . rhs) r

isRewriteRule :: Rule -> Bool
isRewriteRule (Rule l r) = (not $ T.isVariable l) && (T.variables r) `Set.isSubsetOf` (T.variables l)

isDuplicating :: Rule -> Bool
isDuplicating (Rule l r) = any (\x -> T.varCardinality x l < T.varCardinality x r) lvar
  where lvar = Set.toList $ T.variables r

isNonDuplicating :: Rule -> Bool
isNonDuplicating = not . isDuplicating

isFlat :: Rule -> Bool
isFlat = bothsides T.isFlat

isShallow :: Rule -> Bool
isShallow = bothsides T.isShallow

isLinear :: Rule -> Bool
isLinear = bothsides T.isLinear

isGround :: Rule -> Bool
isGround = bothsides T.isGround

isLeftFlat :: Rule -> Bool
isLeftFlat = T.isFlat . lhs

isRightFlat :: Rule -> Bool
isRightFlat = T.isFlat . rhs

isLeftShallow :: Rule -> Bool
isLeftShallow = T.isShallow . lhs

isRightShallow :: Rule -> Bool
isRightShallow = T.isShallow . rhs

isLeftLinear :: Rule -> Bool
isLeftLinear = T.isLinear . lhs

isRightLinear :: Rule -> Bool
isRightLinear = T.isLinear . rhs

isLeftGround :: Rule -> Bool
isLeftGround = T.isGround . lhs

isRightGround :: Rule -> Bool
isRightGround = T.isGround . rhs