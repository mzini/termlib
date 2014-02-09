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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Termlib.FunctionSymbol
where
import qualified Termlib.Signature as Sig
import Termlib.Utils (PrettyPrintable(..), Enumerateable(..))
import Text.PrettyPrint.HughesPJ
import Data.Typeable
import qualified Data.Set as Set
type FunctionName = String
type Arity = Int

data Symbol = Symbol !Int deriving (Eq, Ord, Show, Typeable)

instance Enumerateable Symbol where
  enum (Symbol i) = i
  invEnum = Symbol

data Label = NatLabel Int
           | RootLabel [Symbol] deriving (Eq, Ord, Show)

data Attributes = Attributes { symIdent :: !FunctionName
                             , symArity :: !Arity
                             , symIsMarked :: !Bool
                             , symIsCompound :: !Bool
                             , symLabel :: Maybe Label}
                  deriving (Eq, Show)

type Signature = Sig.Signature Symbol Attributes

type SignatureMonad = Sig.SignatureMonad Symbol Attributes

defaultAttribs :: FunctionName -> Arity -> Attributes
defaultAttribs name ar  = Attributes { symIdent = name
                                     , symArity = ar
                                     , symIsMarked = False
                                     , symIsCompound = False
                                     , symLabel = Nothing}

emptySignature :: Signature
emptySignature = Sig.empty

symbol :: FunctionName -> Signature -> Maybe Symbol
symbol name sig = Sig.findByAttribute p sig
  where p attrib = symIdent attrib == name

isSymbol :: Attributes -> Signature -> Bool
isSymbol attribs sig = Sig.findByAttribute ((==) attribs) sig /= Nothing

fresh :: Attributes -> SignatureMonad Symbol
fresh = Sig.fresh

maybeFresh :: Attributes -> SignatureMonad Symbol
maybeFresh = Sig.maybeFresh

getAttributes :: Symbol -> SignatureMonad Attributes
getAttributes = Sig.getAttributes

symbolName :: Signature -> Symbol -> FunctionName
symbolName = flip $ Sig.attribute symIdent 

lookup :: Symbol -> Signature -> Maybe Attributes
lookup s sig = Sig.lookup (enum s) sig

arity :: Signature -> Symbol -> Arity
arity = flip $ Sig.attribute symArity 

isCompound :: Signature -> Symbol -> Bool
isCompound = flip $ Sig.attribute symIsCompound

isMarked :: Signature -> Symbol -> Bool
isMarked = flip $ Sig.attribute symIsMarked

symbolLabel :: Signature -> Symbol -> Maybe Label
symbolLabel = flip $ Sig.attribute symLabel

argumentPositions :: Signature -> Symbol -> [Int]
argumentPositions sig sym  = case arity sig  sym of
                              0 -> []
                              n -> [1..n]

restrictToSymbols :: Signature -> Set.Set Symbol -> Signature
restrictToSymbols = Sig.restrictToSymbols

symbols :: Signature -> Set.Set Symbol
symbols = Sig.symbols

instance PrettyPrintable (Symbol, Signature) where
  pprint (sym,sig) = ppname <> ppmark <> pplabel  
    where ppname = text $ symbolName sig sym
          ppmark = if isMarked sig sym then text "^#" else empty
          pplabel = case symbolLabel sig sym of 
                      Just (NatLabel l)  -> text "_" <> int l
                      Just (RootLabel []) -> empty
                      Just (RootLabel [s]) -> text "_" <> pprint (s,sig)
                      Just (RootLabel l) -> text "_" <> (parens $ hcat $ punctuate (text ",") [pprint (s,sig) | s <- l])
                      Nothing            -> empty
