{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Problem.XmlParser where

import Control.Monad.RWS.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import Control.Monad.Error
import Termlib.Problem
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Rule as R
import qualified Termlib.Variable as V
import Termlib.Variable (Variables)
import qualified Termlib.Signature as Signature
import Termlib.FunctionSymbol (Signature, Symbol, emptySignature)
import Termlib.Term (Term(..))
import qualified Termlib.Trs as Trs
import Termlib.Trs (Trs)
import Termlib.Problem.Parser


type SymMap = Map String Symbol

newtype Parser r = P { 
    runParser :: ErrorT ParseError (RWS (Maybe SymMap) [ParseWarning] V.Variables) r
  } deriving (Monad, MonadError ParseError)

liftP ::  RWS (Maybe SymMap) [ParseWarning] V.Variables a -> Parser a
liftP m = P $ lift m

sym :: String -> Parser F.Symbol
sym name = do ms <- liftP $ ask >>= return . Map.lookup name . fromMaybe (error "reading symbol befor parsing signature!") 
              case ms of 
                Just s -> return s
                Nothing  -> throwError $ SymbolNotInSignature name

var :: String -> Parser V.Variable
var name = liftP $ do vars <- get
                      let (v, vars') = Signature.runSignature (V.maybeFresh name) vars
                      put vars'
                      return v

warn :: ParseWarning -> Parser ()
warn a = liftP $ tell [a]

cElems :: CFilter
cElems = f `o` children
  where f a@(CElem _) = keep a 
        f a           = none a

parseOne :: a -> (b -> a) -> [b] -> a
parseOne _ p [a] = p a
parseOne e _ _   = e

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString str = case evalRWS run Nothing V.emptyVariables of 
                          (Left e,_) -> Left e
                          (Right r,w) -> Right (r,w)
  where doc = getContent $ xmlParse ".xml problem input" str  
        getContent (Document _ _ e _) = CElem e
        run = runErrorT $ runParser $ parseProblem doc

parseProblem :: Content -> Parser Problem
parseProblem doc = do (symMap, sig) <- parseOne errSig parseSignature $ tag "problem" /> tag "trs" /> tag "signature" $ doc
                      (strict,weak, vars) <- parseOne errTrs (parseTrs sig symMap) $ tag "problem" /> tag "trs" /> tag "rules" $ doc
                      strategy <- parseStrategy doc
                      let both    =  strict `Trs.union` weak
                          constrs = Trs.definedSymbols both
                          defs    = Trs.constructors both
                      startTerms <- parseStartTerms defs constrs doc
                      case Trs.isEmpty weak of  
                        True -> return $ standardProblem startTerms strategy strict vars sig
                        False -> return $ relativeProblem startTerms strategy strict weak vars sig
  where errSig = throwError $ UnknownError "Error when parsing signature"
        errTrs = throwError $ UnknownError "Error when parsing trs"

parseSignature :: Content -> Parser (SymMap, Signature)
parseSignature = liftM mkSig . parseSymbolList
  where mkSig attribs = Signature.runSignature (foldl mk (return Map.empty) attribs) emptySignature 
        mk m attrib  = do map <- m 
                          sym <- F.fresh attrib
                          return $ Map.insert (F.symIdent attrib) sym map
        parseSymbolList :: MonadError ParseError m => Content -> m [F.Attributes]
        parseSymbolList doc = return $ map getAttribs $ getSymbols doc
          where getSymbols = tag "signature" /> tag "funcsym"
                getAttribs c = F.defaultAttribs (vtg "name") (read $ vtg "arity")
                  where vtg t = verbatim $ tag "funcsym" /> tag t /> txt $ c

parseStrategy :: Content -> Parser Strategy
parseStrategy doc = case verbatim $ tag "problem" /> tag "strategy" /> txt $ doc of
                      "INNERMOST" -> return Innermost
                      "OUTERMOST" -> warn (PartiallySupportedStrategy "OUTERMOST") >> return Full
                      "FULL" -> return Full
                      a -> throwError $ UnsupportedStrategy a

parseStartTerms :: Set F.Symbol -> Set F.Symbol -> Content -> Parser StartTerms
parseStartTerms defs constrs doc = case tag "problem" /> tag "startterm" /> txt $ doc of
                                     [] -> return TermAlgebra
                                     _ -> return $ BasicTerms defs constrs
                               
parseTrs :: Signature -> SymMap -> Content -> Parser (Trs,Trs, Variables)
parseTrs sig syms doc = P $ local (const $ Just syms) $ runParser $ parseRules sig doc

parseRules :: Signature -> Content -> Parser (Trs,Trs, Variables)
parseRules sig doc = do strict <- mapM parseRule $ tag "rules" /> tag "rule" $ doc
                        weak <- mapM parseRule $ tag "rules" /> tag "relrules" /> tag "rule" $ doc
                        vars <- liftP get
                        return (Trs.fromRules strict, Trs.fromRules weak, vars)       

parseRule :: Content -> Parser R.Rule
parseRule doc = liftM2 R.Rule parseLhs parseRhs
  where parseLhs,parseRhs :: Parser Term
        parseLhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "lhs") $ doc
        parseRhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "rhs") $ doc
        err = throwError $ MalformedRule doc

parseTerm :: Content -> Parser Term
parseTerm doc = maybe err id (pv `mplus` pt)
  where pt = case verbatim $ tag "funapp" /> tag "name" /> txt $ doc of 
               "" -> Nothing
               s  -> Just $ liftM2 Fun (sym s) (forM args parseTerm)
                 where args = cElems `o` (tag "funapp" /> tag "arg") $ doc
        pv = case tag "var" $ doc of 
               [v] -> Just $  (var $ verbatim $ tag "var" /> txt $ v) >>= return . Var
               _ -> Nothing
        err    = throwError $ MalformedTerm doc



  