module Termlib.Variable where

import Termlib.Signature hiding (fresh, maybeFresh)
import qualified Termlib.Signature as Signature
import Termlib.Utils
import Text.PrettyPrint.HughesPJ (text)

data Variable = Canon !Int
              | User !Int  deriving (Eq, Ord, Show)

instance Enumerateable Variable where
  enum (Canon i) = 2 * i
  enum (User i) = 2 * i + 1
  invEnum i | even i    = Canon $ i `div` 2 
            | otherwise = User $ (i - 1) `div` 2

data Attributes = Attributes {ident :: String}
                  deriving (Eq, Show)

type Variables = Signature Variable Attributes

defaultAttribs :: String -> Attributes
defaultAttribs name  = Attributes {ident = name}

emptyVariables :: Variables
emptyVariables = empty

variable :: String -> Variables -> Maybe Variable
variable name sig = findByAttribute p sig
  where p attrib = ident attrib == name 

isVariable :: String -> Variables -> Bool
isVariable name vars = findByAttribute ((==) $ Attributes name) vars /= Nothing

fresh :: String -> SignatureMonad Variable Attributes Variable
fresh n = Signature.fresh $ Attributes n

maybeFresh :: String -> SignatureMonad Variable Attributes Variable
maybeFresh n = Signature.maybeFresh $ Attributes n

lookup :: Variable -> Variables -> Maybe Attributes
lookup v sig = Termlib.Signature.lookup (enum v) sig

variableName :: Variable -> Variables -> String
variableName = attribute ident

instance PrettyPrintable Attributes where
  pprint = text . ident
