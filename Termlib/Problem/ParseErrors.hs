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
{-# LANGUAGE ExistentialQuantification #-}

module Termlib.Problem.ParseErrors
    ( ParseError(..)
    , ParseWarning(..)
    )
where

import Control.Monad.Error
import Termlib.Utils (PrettyPrintable(..))
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml
import qualified Text.ParserCombinators.Parsec as Parsec 

data ParseError = forall i. MalformedTerm (Content i)
                | forall i. MalformedRule (Content i)
                | UnknownError String
                | UnsupportedStrategy String
                | SymbolNotInSignature String
                | ParsecParseError Parsec.ParseError
                | UnknownFileError String
                | ProblemNotFoundError String
                | UnsupportedRewritingError String

instance Error ParseError where
  strMsg = UnknownError

data ParseWarning = PartiallySupportedStrategy String
                  | PartiallySupportedStartTerms String
                  | ContextSensitive deriving Show


instance PrettyPrintable ParseError where
  pprint (MalformedTerm s) = text "Malformed term" $$ text (verbatim s)
  pprint (MalformedRule s) = text "Malformed rule" $$ text (verbatim s)
  pprint (SymbolNotInSignature s) = text "Symbol" <+>  quotes (text s) <+> text "not referenced in the signature"
  pprint (UnsupportedStrategy s) = text "Unsupported strategy" <+> quotes (text s)
  pprint (UnknownError e) = text "Unknown error" <+> text e
  pprint (ParsecParseError e) = text $ show e
  pprint (UnknownFileError e) = text "Don't know how to parse file " <+> text e
  pprint (ProblemNotFoundError s) = text "Error when opening problem file" <+> quotes (text s)
  pprint (UnsupportedRewritingError s) = text "Unsupported rewriting variant" <+> quotes (text s)

instance PrettyPrintable ParseWarning where 
  pprint (PartiallySupportedStrategy s) = text "Unsupported strategy" <+> quotes (text s)
  pprint (PartiallySupportedStartTerms s) = text "Unsupported set of start terms" <+> quotes (text s)
  pprint ContextSensitive = text "Contextsensitive signature not supported"
