{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Termlib.FunctionSymbol
where
import qualified Termlib.Signature as Sig
import Termlib.Utils (PrettyPrintable(..), Enumerateable(..))
import Text.PrettyPrint.HughesPJ
import Data.Typeable
type FunctionName = String
type Arity = Int

data Symbol = Symbol !Int deriving (Eq, Ord, Show, Typeable)

instance Enumerateable Symbol where
  enum (Symbol i) = i
  invEnum = Symbol

data Attributes = Attributes { symIdent :: !FunctionName
                             , symArity :: !Arity
                             , symIsMarked :: !Bool
                             , symIsCompound :: !Bool
                             , symLabel :: Maybe Int}
                  deriving (Eq, Show)

type Signature = Sig.Signature Symbol Attributes

defaultAttribs :: FunctionName -> Arity -> Attributes
defaultAttribs name ar  = Attributes { symIdent = name
                                     , symArity = ar
                                     , symIsMarked = False
                                     , symIsCompound = False
                                     , symLabel = Nothing}

isSymbol :: Attributes -> Signature -> Bool
isSymbol = Sig.elemAttrib

symbol :: FunctionName -> Signature -> Maybe Symbol
symbol name sig = Sig.findByAttribute p sig
  where p attrib = symIdent attrib == name

emptySignature :: Signature
emptySignature = Sig.empty

fresh :: Signature -> Attributes -> (Symbol, Signature)
fresh = flip Sig.fresh

getSymbol :: Signature -> Attributes -> (Symbol, Signature)
getSymbol = flip Sig.fromAttrib

symbolName :: Signature -> Symbol -> FunctionName
symbolName = flip $ Sig.attribute symIdent 

arity :: Signature -> Symbol -> Arity
arity = flip $ Sig.attribute symArity 

isCompound :: Signature -> Symbol -> Bool
isCompound = flip $ Sig.attribute symIsCompound

isMarked :: Signature -> Symbol -> Bool
isMarked = flip $ Sig.attribute symIsMarked

argumentPositions :: Signature -> Symbol -> [Int]
argumentPositions sig sym  = case arity sig  sym of
                              0 -> []
                              n -> [1..n]

instance PrettyPrintable Attributes where
  pprint attribs = ppname <> ppmark <> pplabel  
    where ppname = text $ symIdent attribs
          ppmark = if symIsMarked attribs then text "^#" else empty
          pplabel = case symLabel attribs of 
                      Just l  -> text "_" <> int l
                      Nothing -> empty
