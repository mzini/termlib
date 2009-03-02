module Termlib.FunctionSymbol
where
import qualified Termlib.Signature as Sig
import Termlib.Utils

type Arity = Int
type FunctionName = String

newtype Symbol = Symbol Int deriving (Eq, Ord, Show)

instance Enumerateable Symbol where
  enum (Symbol i) = i
  invEnum = Symbol

data Attributes = Attributes { ident :: FunctionName
                             , arity :: Arity
                             , isMarked :: Bool
                             , isCompound :: Bool
                             , label :: Maybe Int}
                  deriving Show 

type Signature = Sig.Signature Symbol Attributes

defaultAttribs :: FunctionName -> Arity -> Attributes
defaultAttribs name ar  = Attributes { ident = name
                                     , arity = ar
                                     , isMarked = False
                                     , isCompound = False
                                     , label = Nothing}

symbol :: String -> Signature -> Maybe Symbol
symbol name sig = Sig.findByAttribute p sig
  where p attrib = ident attrib == name

emptySignature :: Signature
emptySignature = Sig.empty

fresh :: Attributes -> Signature -> (Symbol, Signature)
fresh = Sig.fresh