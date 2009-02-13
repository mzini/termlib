module Termlib.Substitution
  (
   empty,
   singleton,
   map,
   union,
   lookup,
   apply,
   compose,
   subsumes,
   match,
   variant,
   Substitution
  ) where

import qualified Termlib.Term as T
import qualified Termlib.Variable as V
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Prelude hiding (lookup, map)
newtype Substitution = Substitution (Map.Map V.Variable T.Term)

map f s@(Substitution sub) = Substitution $ fmap f sub

empty = Substitution Map.empty

singleton x = Substitution . Map.singleton x

s@(Substitution s') `union` t@(Substitution t') = Substitution (s' `Map.union` t')

lookup x s@(Substitution sub) = Map.lookup x sub

apply s (T.Var x) = Maybe.fromMaybe (T.Var x) (lookup x s)
apply s (T.Fun f xs) = T.Fun f (fmap (apply s) xs)

compose s@(Substitution s') t@(Substitution t') = (map (apply t) s) `union` t

s `subsumes` t = Maybe.isJust (match t s empty)

match s (T.Var x) sub = Maybe.maybe (Just (compose sub (singleton x s)))
                        (\t -> if s == t then Just sub else Nothing) (lookup x sub)
                                               
match (T.Fun g ys) (T.Fun f xs) sub
  | f == g && length xs == length ys = foldr doSubTerm (Just sub) (zip ys xs)
  | otherwise                        = Nothing
  where doSubTerm (y, x) s' = maybe Nothing (\sub' -> match y x sub') s'
match _ _ _ = Nothing

variant s t = (s `subsumes` t) && (t `subsumes` s)
