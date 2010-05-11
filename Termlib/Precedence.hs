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
  pprint (Precedence (sig,l)) = fsep $ punctuate (text ",") [pp e | e <- l] 
    where pp (f :>: g) = pprint (f,sig) <+> text ">" <+> pprint (g,sig)
          pp (f :~: g) =  pprint (f,sig) <+> text "~" <+> pprint (g,sig)

precedence :: Signature -> [Order] -> Precedence
precedence = curry Precedence 

empty :: Signature -> Precedence
empty sig = precedence sig []

insert :: Order -> Precedence -> Precedence
insert e (Precedence (sig, l)) = Precedence (sig, e : l)

