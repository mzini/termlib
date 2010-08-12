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

import Control.Exception (assert)
import qualified Termlib.Substitution as S
import qualified Termlib.Term as T
import Termlib.Term (Term)
import Termlib.Variable (Variable)
import Termlib.FunctionSymbol (Symbol)
import Termlib.Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Maybe as Maybe
import Text.PrettyPrint.HughesPJ hiding (empty)

data Rule = Rule {lhs :: Term, rhs :: Term} deriving Show

data Strictness = StrictRule | WeakRule

instance PrettyPrintable Rule where
    pprint (Rule l r) = pprint l <+> text "->" <+> pprint r

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

rewriteAnyRhs :: [Rule] -> [Rule] -> Maybe (Rule, Rule)
rewriteAnyRhs ss rs = Maybe.listToMaybe $ Maybe.mapMaybe (flip rewriteRhsAnyRule rs) ss

rewriteRhsAnyRule :: Rule -> [Rule] -> Maybe (Rule, Rule)
rewriteRhsAnyRule r = Maybe.listToMaybe . Maybe.mapMaybe (f $ rewrite $ rhs r)
                                where l      = lhs r
                                      f g r' = case g r' of
                                                 Nothing      -> Nothing
                                                 Just (s, s') -> assert (s == rhs r) $ Just (Rule l s, Rule l s')

rewriteAny :: [Term] -> [Rule] -> Maybe (Term, Term)
rewriteAny ts rs = Maybe.listToMaybe $ Maybe.mapMaybe (flip rewriteAnyRule rs) ts

rewriteAnyRule :: Term -> [Rule] -> Maybe (Term, Term)
rewriteAnyRule t = Maybe.listToMaybe . Maybe.mapMaybe (rewrite t)

rewrite :: Term -> Rule -> Maybe (Term, Term)
rewrite (T.Var _) _      = Nothing
rewrite t@(T.Fun f ts) r = case topRewrite t r of
                               Nothing -> subresult [] ts
                               Just t' -> Just t'
  where subresult _   []       = Nothing
        subresult ts1 (t':ts2) = case rewrite t' r of
                                   Nothing      -> subresult (ts1 ++ [t']) ts2
                                   Just (s, s') -> assert (t' == s) Just (T.Fun f (ts1 ++ s:ts2), T.Fun f (ts1 ++ s':ts2))

rewriteCandidates :: Term -> Rule -> [(Term, Term)]
rewriteCandidates (T.Var _)      _ = []
rewriteCandidates t@(T.Fun f ts) r = case topRewrite t r of
                                       Nothing -> subresult [] ts
                                       Just t' -> t' : subresult [] ts
  where subresult _   []       = []
        subresult ts1 (t':ts2) = map (\(s, s') -> (applyContext f ts1 ts2 s, applyContext f ts1 ts2 s')) (rewriteCandidates t' r) ++ subresult (ts1 ++ [t']) ts2
        applyContext f' ts1 ts2 s = T.Fun f' (ts1 ++ s:ts2)

topRewrite :: Term -> Rule -> Maybe (Term, Term)
topRewrite t (Rule l r) = case S.match t l S.empty of
                            Nothing  -> Nothing
                            Just sub -> Just (t, S.apply sub r)

reduced :: Term -> Rule -> Bool
reduced s@(T.Var _) r = topReduced s r
reduced s@(T.Fun _ xs) r = topReduced s r && all (`reduced` r) xs

topReduced :: Term -> Rule -> Bool
topReduced s r = not (lhs r `S.subsumes` s)

bothsides :: (Term -> Bool) -> Rule -> Bool
bothsides f r = (f . lhs) r && (f . rhs) r

isRewriteRule :: Rule -> Bool
isRewriteRule (Rule l r) = (not $ T.isVariable l) && (T.variables r) `Set.isSubsetOf` (T.variables l)

isErasing :: Rule -> Bool
isErasing = not . isNonErasing

isNonErasing :: Rule -> Bool
isNonErasing (Rule l r) = T.variables l == T.variables r

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

isCollapsing :: Rule -> Bool
isCollapsing = T.isVariable . rhs

isOverlapping :: Rule -> Rule -> Bool
isOverlapping r1 r2 = any (S.isRenamedUnifiable l2) (appropriateSubterms l1) || any (S.isRenamedUnifiable l1) (appropriateSubterms l2)
  where l1 = lhs r1
        l2 = lhs r2
        appropriateSubterms = if r1 == r2 then T.properNonVariableSubterms else T.nonVariableSubterms

isAnyOverlapping :: [Rule] -> [Rule] -> Bool
isAnyOverlapping rs1 rs2 = any (\r -> any (isOverlapping r) rs2) rs1
