module Termlib.Term
  (
   canonise,
   depth,
  functions,
  root,
  size,
  fsize,
  vardepth,
  flat,
  shallow,
  linear,
  ground,
  immediateSubterms,
  subterm,
  superterm,
  variables,
  isVariable,
  varCardinality,
  Term(..)
  ) where

import qualified Termlib.Variable as V
import qualified Termlib.FunctionSymbol as F
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Term = Var V.Variable | Fun F.Symbol [Term]
  deriving (Eq, Ord, Show)

depth (Var _) = 0
depth (Fun _ []) = 0
depth (Fun _ xs) = (succ . maximum . map depth) xs

root (Var v) = Left v
root (Fun f _) = Right f

size (Var _) = 1
size (Fun _ xs) = (succ . sum . map size) xs

fsize (Var _) = 0
fsize (Fun _ xs) = (succ . sum . map fsize) xs

vardepth (Var _) = Just 0
vardepth (Fun _ xs) = if subresults == [] then Nothing else (Just . succ . maximum) subresults
  where subresults = (Maybe.catMaybes . map vardepth) xs

flat t = depth t <= 1

shallow = maybe True (<= 1) . vardepth

linear t = List.nub vs == vs
  where vs = variables t

ground = (== []) . variables

functions (Var _) = []
functions (Fun f xs) = (List.nub . (:) f . concatMap functions) xs

immediateSubterms (Var _) = []
immediateSubterms (Fun _ xs) = xs

s `subterm` t = s == t || (any (s `subterm`) . immediateSubterms) t

superterm = flip subterm

variables (Var v) = [v]
variables (Fun _ xs) = (List.nub . concatMap variables) xs

isVariable (Var _) = True
isVariable (Fun _ _) = False

varCardinality x (Var y) | x == y    = 1
                         | otherwise = 0
varCardinality x (Fun _ xs) = (sum . map (varCardinality x)) xs

canonise (Var x) varmap = case Map.lookup x varmap of
  Nothing -> addvar x varmap
    where addvar v vm = let newelem = undefined in
  -- MA:TODO sry (V.freshVar . Map.elems) vm in
            (Var newelem, Map.insert v newelem vm)
  Just oldelem -> (Var oldelem, varmap)
canonise (Fun f xs) varmap = (Fun f (fst subresult), snd subresult)
  where subresult = foldl doSubTerm ([], varmap) xs
        doSubTerm a t = (fst a ++ [fst subsubresult], snd subsubresult)
          where subsubresult = canonise t (snd a)