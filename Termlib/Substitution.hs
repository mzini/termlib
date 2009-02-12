module Substitution
  (
  empty,
  singleton,
  Substitution.map,
  union,
  Substitution.lookup,
  apply,
  compose,
  subsumes,
  match,
  variant,
  Substitution
  ) where

import qualified Term as T
import qualified Variable as V
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

newtype Substitution = Substitution (Map.Map V.Variable T.Term)

empty = Substitution Map.empty

singleton x = Substitution . Map.singleton x

map f s@(Substitution sub) = Substitution (Map.map f sub)

s@(Substitution s') `union` t@(Substitution t') = Substitution (s' `Map.union` t')

lookup x s@(Substitution sub) = Map.lookup x sub

apply s (T.Var x) = Maybe.fromMaybe (T.Var x) (Substitution.lookup x s)
apply s (T.Fun f xs) = T.Fun f (Prelude.map (apply s) xs)

compose s@(Substitution s') t@(Substitution t') = (Substitution.map (apply t) s) `union` t

s `subsumes` t = Maybe.isJust (match t s empty)

match s (T.Var x) sub = Maybe.maybe (Just (compose sub (singleton x s)))
  (\t -> if s == t then Just sub else Nothing) (Substitution.lookup x sub)

variant s t = (s `subsumes` t) && (t `subsumes` s)