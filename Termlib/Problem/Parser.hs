module Termlib.Problem.Parser where
import Termlib.Utils (PrettyPrintable(..))
import Text.PrettyPrint.HughesPJ
import Control.Monad.Error

import Text.XML.HaXml
import qualified Text.ParserCombinators.Parsec as Parsec 

data ParseError = MalformedTerm Content
                | MalformedRule Content
                | UnknownError String
                | UnsupportedStrategy String
                | SymbolNotInSignature String
                | ParsecParseError Parsec.ParseError

instance Error ParseError where
  strMsg = UnknownError

data ParseWarning = PartiallySupportedStrategy String
                  | ContextSensitive deriving Show


instance PrettyPrintable ParseError where
  pprint (MalformedTerm s) = text "Malformed term" $$ text (verbatim s)
  pprint (MalformedRule s) = text "Malformed rule" $$ text (verbatim s)
  pprint (SymbolNotInSignature s) = text "Symbol" <+>  quotes (text s) <+> text "not referenced in the signature"
  pprint (UnsupportedStrategy s) = text "Unsupported strategy" <+> quotes (text s)
  pprint (UnknownError e) = text "Unknown error" <+> text e
  pprint (ParsecParseError e) = text $ show e

instance PrettyPrintable ParseWarning where 
  pprint (PartiallySupportedStrategy s) = text "Unsupported strategy" <+> quotes (text s)
  pprint ContextSensitive = text "Contextsensitive signature not supported"
