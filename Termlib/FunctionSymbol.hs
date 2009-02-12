module FunctionSymbol
  (
  name,
  Sig,
  FunctionSymbol
  ) where

import qualified Data.IntMap as IntMap

newtype FunctionSymbol = FunctionSymbol Int deriving (Eq, Show)

data Prop = Prop {
  ident :: Maybe String,
  arity :: Int,
  isMarked :: Bool,
  isCompound :: Bool,
  label :: Maybe Int
  }

newtype Sig = Sig (IntMap.IntMap Prop)

name (FunctionSymbol f) sig = case IntMap.lookup f sig of
  Nothing -> error $ "FunctionSymbol.name: function symbol not contained in signature: " ++ (show f)
  Just p -> maybe ("f_" ++ show f) id $ ident p