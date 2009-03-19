{-# LANGUAGE DeriveDataTypeable #-}

module Termlib.Precedence where
import Termlib.FunctionSymbol 
import Termlib.Signature hiding (Signature)
import Text.PrettyPrint.HughesPJ
import Termlib.Utils (PrettyPrintable(..))
import Data.Typeable 

data Order = Symbol :>: Symbol 
           | Symbol :~: Symbol deriving (Show, Eq, Ord, Typeable)

newtype Precedence = Precedence (Signature,[Order]) deriving Show

instance PrettyPrintable Precedence where 
  pprint (Precedence (_, [])) = text "empty"
  pprint (Precedence (sig,l)) = fsep [pp e | e <- l] 
    where pp (f :>: g)    = pprint (attributes f sig) <+> text ">" <+> pprint (attributes g sig)
          pp (f :~: g) =  pprint (attributes f sig) <+> text "~" <+> pprint (attributes g sig)

precedence :: Signature -> [Order] -> Precedence
precedence = curry Precedence 

empty :: Signature -> Precedence
empty sig = precedence sig []

insert :: Order -> Precedence -> Precedence
insert e (Precedence (sig, l)) = Precedence (sig, e : l)

