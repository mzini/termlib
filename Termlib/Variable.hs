{-# LANGUAGE FlexibleInstances #-}
module Termlib.Variable where

import Termlib.Signature hiding (fresh, maybeFresh)
import qualified Termlib.Signature as Signature
import Termlib.Utils
import Control.Monad (liftM)
import Text.PrettyPrint.HughesPJ (text)

data Variable = Canon !Int
              | User !Int  deriving (Eq, Ord, Show)

instance Enumerateable Int where
  enum = id
  invEnum = id

data Attributes = Attributes {ident :: String}
                  deriving (Eq, Show)

type Variables = Signature Int Attributes
type VariableMonad a = SignatureMonad Int Attributes a

defaultAttribs :: String -> Attributes
defaultAttribs name  = Attributes {ident = name}

emptyVariables :: Variables
emptyVariables = empty

variable :: String -> Variables -> Maybe Variable
variable name sig = User `liftM` findByAttribute p sig
    where p attrib = ident attrib == name 

isVariable :: String -> Variables -> Bool
isVariable name vars = findByAttribute ((==) $ Attributes name) vars /= Nothing

fresh :: String -> VariableMonad Variable
fresh n = User `liftM` (Signature.fresh $ Attributes {ident = n})

maybeFresh :: String -> VariableMonad Variable
maybeFresh n = User `liftM` (Signature.maybeFresh $ Attributes {ident = n})

canonical :: Int -> Variable
canonical i = Canon i

canonVarName :: Int -> String
canonVarName i = "x_" ++ show i

lookup :: Variable -> Variables -> Maybe Attributes
lookup (User v)  sig = Termlib.Signature.lookup v sig
lookup (Canon i) _   = Just $ defaultAttribs $ canonVarName i

variableName :: Variable -> Variables -> String
variableName (User v)  sig = attribute ident v sig
variableName (Canon i) _   = canonVarName i

instance PrettyPrintable (Variable,Variables) where
  pprint (v,vs) = text $ variableName v vs
