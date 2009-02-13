module Termlib.Variable
  (
  name,
  freshVar,
  Sig,
  Variable
  ) where

import qualified Data.IntMap as IntMap

newtype Variable = Variable Int deriving (Eq, Ord, Show)

newtype Prop = Prop (Maybe String)

newtype Sig = Sig (IntMap.IntMap Prop)

name (Variable v) sig = case IntMap.lookup v sig of
  Nothing -> error $ "Variable.name: variable not contained in signature: " ++ (show v)
  Just p -> maybe ("v_" ++ show v) id p

freshVar [] = Variable 1
freshVar xs = (Variable . succ . maximum . map vindex) xs
  where vindex (Variable v) = v