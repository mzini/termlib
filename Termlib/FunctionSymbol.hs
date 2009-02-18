module Termlib.FunctionSymbol
  (
  name,
  Sig,
  FunctionSymbol
  ) where

import Data.Maybe (fromMaybe)
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
  Nothing -> error $ "FunctionSymbol.name: function symbol not contained in signature: " ++ show f
  Just p -> fromMaybe ("f_" ++ show f) $ ident p