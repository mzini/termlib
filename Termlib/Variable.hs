module Termlib.Variable where

import Termlib.Signature hiding (fresh)
import qualified Termlib.Signature as Signature
import Termlib.Utils

data Variable = Canon !Int 
              | User !Int  deriving (Eq, Ord, Show)

instance Enumerateable Variable where
  enum (Canon i) = 2 * i
  enum (User i) = 2 * i + 1
  invEnum i | even i    = Canon $ i `div` 2 
            | otherwise = User $ (i - 1) `div` 2

data Attributes = Attributes {ident :: String}
                  deriving Show 

type Variables = Signature Variable Attributes

defaultAttribs :: String -> Attributes
defaultAttribs name  = Attributes {ident = name}

variable :: String -> Variables -> Maybe Variable
variable name sig = findByAttribute p sig
  where p attrib = ident attrib == name 

emptyVariables :: Variables
emptyVariables = empty

fresh :: String  -> Variables -> (Variable, Variables)
fresh n = Signature.fresh $ Attributes $ n

