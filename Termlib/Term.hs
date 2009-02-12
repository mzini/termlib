module Term
  (
  depth,
  root,
  size,
  Term(..)
  ) where

import qualified Variable as V
import qualified FunctionSymbol as F

data Term = Var V.Variable | Fun F.FunctionSymbol [Term]

depth (Var _) = 0
depth (Fun _ []) = 0
depth (Fun _ xs) = 1 + maximum $ map depth xs

root (Var v) = Left v
root (Fun f _) = Right f

size (Var _) = 1
size (Fun _ xs) = sum $ map size xs