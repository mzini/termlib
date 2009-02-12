module Term
  (
  canonise,
  depth,
  root,
  size,
  Term(..)
  ) where

import qualified Variable as V
import qualified FunctionSymbol as F
import qualified Data.Map as Map

data Term = Var V.Variable | Fun F.FunctionSymbol [Term]
  deriving (Eq, Show)

depth (Var _) = 0
depth (Fun _ []) = 0
depth (Fun _ xs) = 1 + maximum $ map depth xs

root (Var v) = Left v
root (Fun f _) = Right f

size (Var _) = 1
size (Fun f xs) = sum $ map size xs

canonise (Var x) varmap = case Map.lookup x varmap of
  Nothing -> addvar x varmap
    where addvar v vm = let newelem = (V.freshVar . Map.elems) vm in
            (Var newelem, Map.insert v newelem vm)
  Just oldelem -> (Var oldelem, varmap)
canonise (Fun f xs) varmap = (Fun f (fst subresult), snd subresult)
  where subresult = foldl doSubTerm ([], varmap) xs
        doSubTerm a t = ((fst a) ++ [fst subsubresult], snd subsubresult)
          where subsubresult = canonise t (snd a)

variant s t = (canonise s Map.empty) == (canonise t Map.empty)