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
   isUnifiable,
   encompasses,
   variant,
   Substitution
  ) where

import Control.Monad.State.Lazy as State
import qualified Termlib.Term as T
import qualified Termlib.Variable as V
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Prelude hiding (lookup, map)
newtype Substitution = Substitution (Map.Map V.Variable T.Term)

map f s@(Substitution sub) = Substitution $ fmap f sub

empty = Substitution Map.empty

singleton x = Substitution . Map.singleton x

s@(Substitution s') `union` t@(Substitution t') = Substitution (s' `Map.union` t')

lookup x s@(Substitution sub) = Map.lookup x sub

apply s (T.Var x) = Maybe.fromMaybe (T.Var x) (lookup x s)
apply s (T.Fun f xs) = T.Fun f (fmap (apply s) xs)

compose s t = map (apply t) s `union` t

s `subsumes` t = Maybe.isJust (match t s empty)

-- we say s matches (the pattern) t if s is an instance of t, ie., t subsumes s
match s (T.Var x) sub = Maybe.maybe (Just (compose sub (singleton x s)))
                        (\t -> if s == t then Just sub else Nothing) (lookup x sub)
match (T.Fun g ys) (T.Fun f xs) sub
    | f == g && length xs == length ys = foldr doSubTerm (Just sub) (zip ys xs)
    | otherwise                        = Nothing
  where doSubTerm (y, x) = maybe Nothing (\sub' -> match y x sub')
match _ _ _ = Nothing

isUnifiable :: T.Term -> T.Term -> Bool
isUnifiable s t = State.evalState unify ([(s, t)], empty)

unify :: State.State ([(T.Term, T.Term)], Substitution) Bool
unify = do (eqs, sub) <- State.get
           if null eqs then return True else
             case head eqs of
               (s, t) | s == t                                   -> do State.put (tail eqs, sub)
                                                                       unify
               (s@(T.Fun _ _), t@(T.Var _))                      -> do State.put ((t, s) : tail eqs, sub)
                                                                       unify
               (T.Fun f ss, T.Fun g ts) | f /= g                 -> return False
                                        | length ss /= length ts -> return False
                                        | otherwise              -> do State.put (zip ss ts ++ tail eqs, sub)
                                                                       unify
               (T.Var x, t) | x `Set.member` T.variables t       -> return False
                            | otherwise                          -> do let subx = singleton x t
                                                                       let eqs' = fmap (fmap $ apply subx) (tail eqs)
                                                                       let sub' = compose sub subx
                                                                       State.put (eqs', sub')
                                                                       unify

variant s t = (s `subsumes` t) && (t `subsumes` s)

s `encompasses` t = t `subsumes` s || any (t `encompasses`) (T.immediateSubterms s)