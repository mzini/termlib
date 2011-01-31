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

{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Problem.XmlParser where

import Control.Monad.Error
import Control.Monad.RWS.Lazy
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Termlib.FunctionSymbol (Signature, Symbol, emptySignature)
import Termlib.Problem
import qualified Termlib.ContextSensitive as CS
import Termlib.Problem.ParseErrors (ParseError (..), ParseWarning (..))
import Termlib.Term (Term(..))
import qualified Termlib.Trs as Trs
import Termlib.Trs (Trs)
import Termlib.Variable (Variables)
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Rule as R
import qualified Termlib.Signature as Signature
import qualified Termlib.Variable as V


type SymMap = Map String Symbol

newtype Parser r = P { 
    runParser :: ErrorT ParseError (RWS (Maybe SymMap) [ParseWarning] V.Variables) r
  } deriving (Monad, MonadError ParseError)

liftP ::  RWS (Maybe SymMap) [ParseWarning] V.Variables a -> Parser a
liftP m = P $ lift m

sym :: String -> Parser F.Symbol
sym symname = do ms <- liftP $ ask >>= return . Map.lookup symname . fromMaybe (error "reading symbol befor parsing signature!") 
                 case ms of 
                   Just s -> return s
                   Nothing  -> throwError $ SymbolNotInSignature symname

var :: String -> Parser V.Variable
var varname = liftP $ do vars <- get
                         let (v, vars') = Signature.runSignature (V.maybeFresh varname) vars
                         put vars'
                         return v

warn :: ParseWarning -> Parser ()
warn a = liftP $ tell [a]

cElems :: CFilter i
cElems = f `o` children
  where f a@(CElem _ _) = keep a
        f a             = none a

parseOne :: a -> (b -> a) -> [b] -> a
parseOne _ p [a] = p a
parseOne e _ _   = e

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString str = case evalRWS run Nothing V.emptyVariables of 
                          (Left e,_) -> Left e
                          (Right r,w) -> Right (r,w)
  where doc = getContent $ xmlParse ".xml problem input" str  
        getContent (Document _ _ e _) = CElem e noPos
        run = runErrorT $ runParser $ parseProblem doc

parseProblem :: Content i -> Parser Problem
parseProblem doc = do (symMap, sig, rm) <- parseOne errSig parseSignature $ tag "problem" /> tag "trs" /> tag "signature" $ doc
                      (strict,weak, vars) <- parseOne errTrs (parseTrs sig symMap) $ tag "problem" /> tag "trs" /> tag "rules" $ doc
                      strategy <- if rm == CS.empty 
                                  then parseStrategy doc
                                  else return $ ContextSensitive rm
                      let both    =  strict `Trs.union` weak
                          cons    = Trs.definedSymbols both
                          defs    = Trs.constructors both
                      st <- parseStartTerms defs cons doc
                      case Trs.isEmpty weak of  
                        True -> return $ standardProblem st strategy strict vars sig
                        False -> return $ relativeProblem st strategy strict weak vars sig
  where errSig = throwError $ UnknownError "Error when parsing signature"
        errTrs = throwError $ UnknownError "Error when parsing trs"

parseSignature :: Content i -> Parser (SymMap, Signature, ReplacementMap)
parseSignature cont = do ((mp, rm), sig) <- mkSig `liftM` parseSymbolList cont
                         return (mp, sig, rm)
  where mkSig attribs = Signature.runSignature (foldl mk (return (Map.empty, CS.empty)) attribs) emptySignature
        mk m (attrib, mrp)  = do (mp, rm) <- m 
                                 f <- F.fresh attrib
                                 let rm' = case mrp of { Just is -> CS.setReplacing f is rm; _ -> rm }
                                 return $ (Map.insert (F.symIdent attrib) f mp, rm')
        parseSymbolList :: MonadError ParseError m => Content i -> m [(F.Attributes, Maybe [Int])]
        parseSymbolList doc = return $ map getAttribs $ getSymbols doc
          where getSymbols = tag "signature" /> tag "funcsym"
                getAttribs c = (attribs, mrp)
                  where vtg t   = verbatim $ tag "funcsym" /> tag t /> txt $ c
                        attribs = F.defaultAttribs (vtg "name") (read $ vtg "arity")
                        mrp     = case tag "funcsym" /> tag "replacementmap" $ c of 
                                    [] -> Nothing 
                                    _   -> Just $ map (read . verbatim) entries
                            where entries = cElems `o` (tag "funcsym" /> tag "replacementmap" /> tag "entry" /> txt) $ c


parseStrategy :: Content i -> Parser Strategy
parseStrategy doc = case verbatim $ tag "problem" /> tag "strategy" /> txt $ doc of
                      "INNERMOST" -> return Innermost
                      "OUTERMOST" -> return Outermost
                      "FULL" -> return Full
                      a -> throwError $ UnsupportedStrategy a

-- MA:TODO: verify
parseStartTerms :: Set F.Symbol -> Set F.Symbol -> Content i -> Parser StartTerms
parseStartTerms defs cons doc = case tag "problem" /> tag "startterm" /> txt $ doc of
                                     [] -> return TermAlgebra
                                     _ -> return $ BasicTerms {defineds = defs, constrs = cons}
                                     -- "full" -> return $ TermAlgebra
                                     -- s -> warn (PartiallySupportedStartTerms s) >> return TermAlgebra
                               
parseTrs :: Signature -> SymMap -> Content i -> Parser (Trs,Trs, Variables)
parseTrs sig syms doc = P $ local (const $ Just syms) $ runParser $ parseRules sig doc

parseRules :: Signature -> Content i -> Parser (Trs,Trs, Variables)
parseRules _   doc = do strict <- mapM parseRule $ tag "rules" /> tag "rule" $ doc
                        weak <- mapM parseRule $ tag "rules" /> tag "relrules" /> tag "rule" $ doc
                        vars <- liftP get
                        return (Trs.fromRules strict, Trs.fromRules weak, vars)       

parseRule :: Content i -> Parser R.Rule
parseRule doc = liftM2 R.Rule parseLhs parseRhs
  where parseLhs,parseRhs :: Parser Term
        parseLhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "lhs") $ doc
        parseRhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "rhs") $ doc
        err = throwError $ MalformedRule doc

parseTerm :: Content i -> Parser Term
parseTerm doc = maybe err id (pv `mplus` pt)
  where pt = case verbatim $ tag "funapp" /> tag "name" /> txt $ doc of 
               "" -> Nothing
               s  -> Just $ liftM2 Fun (sym s) (forM args parseTerm)
                 where args = cElems `o` (tag "funapp" /> tag "arg") $ doc
        pv = case tag "var" $ doc of 
               [v] -> Just $  (var $ verbatim $ tag "var" /> txt $ v) >>= return . Var
               _ -> Nothing
        err    = throwError $ MalformedTerm doc



  