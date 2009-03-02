module Termlib.Signature.XmlParser where

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import qualified Data.Map as Map
import Termlib.FunctionSymbol

signatureFromXml :: Content -> (Signature, Map.Map String Symbol)
signatureFromXml = mkSig . symbolList
  where mkSig = foldr mk (emptySignature, Map.empty)
        mk attrib (sig,m) = (sig', Map.insert (ident attrib) i m)
          where (i,sig') = fresh attrib sig
symbolList :: Content -> [Attributes]
symbolList doc = map getAttribs $ getSymbols doc
  where getSymbols = tag "signature" /> tag "funcsym"
        getAttribs c = defaultAttribs (vtg "name") (read $ vtg "arity")
          where vtg t = verbatim $ tag "funcsym" /> tag t /> txt $ c
