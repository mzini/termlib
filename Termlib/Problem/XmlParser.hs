{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Problem.XmlParser where

import Control.Monad.Error
import Control.Monad.RWS.Lazy
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import Termlib.Problem
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Rule as R
import qualified Termlib.Variable as V
import Termlib.FunctionSymbol (Signature, Symbol)
import Termlib.Term (Term(..))
import qualified Termlib.Trs as Trs



data ParseError = MalformedTerm Content
                | MalformedRule Content
                | UnknownError String
                | UnsupportedStrategy String
                | SymbolNotInSignature String

instance Show ParseError where
  show (MalformedTerm s) = "Malformed term:\n" ++ verbatim s
  show (MalformedRule s) = "Malformed rule:\n" ++ verbatim s
  show (SymbolNotInSignature sym) = "SymbolNotInSignature " ++ sym
  show (UnsupportedStrategy s) = "Unsupported strategy: " ++ s
  show (UnknownError e) = e

instance Error ParseError where
  strMsg = UnknownError

type SymMap = Map String Symbol


cElems :: CFilter
cElems = f `o` children
  where f a@(CElem _) = keep a 
        f a           = none a

parseOne :: a -> (b -> a) -> [b] -> a
parseOne _ p [a] = p a
parseOne e _ _   = e

problemFromString :: String -> Either ParseError Problem
problemFromString str = parseProblem doc
  where doc = getContent $ xmlParse "xml:problem" str  
        getContent (Document _ _ e _) = CElem e

parseProblem :: MonadError ParseError m => Content -> m Problem
parseProblem doc = do (sig,symMap) <- parseOne errSig parseSignature $ tag "problem" /> tag "trs" /> tag "signature" $ doc
                      (strict,weak) <- parseOne errTrs (parseTrs sig symMap) $ tag "problem" /> tag "trs" /> tag "rules" $ doc
                      strategy <- parseStrategy doc
                      startTerms <- parseStartTerms (Trs.definedSymbols strict `Set.union` Trs.definedSymbols weak) doc
                      case Trs.isEmpty weak of  
                        True -> return $ standardProblem startTerms strategy strict
                        False -> return $ relativeProblem startTerms strategy strict weak
  where errSig = throwError $ UnknownError "Error when parsing signature"
        errTrs = throwError $ UnknownError "Error when parsing trs"

parseSignature :: MonadError ParseError m => Content -> m (Signature, SymMap)
parseSignature = liftM mkSig . parseSymbolList
  where mkSig = foldr mk (F.emptySignature, Map.empty)
        mk attrib (sig,m) = (sig', Map.insert (F.ident attrib) i m)
          where (i,sig') = F.fresh attrib sig
        parseSymbolList :: MonadError ParseError m => Content -> m [F.Attributes]
        parseSymbolList doc = return $ map getAttribs $ getSymbols doc
          where getSymbols = tag "signature" /> tag "funcsym"
                getAttribs c = F.defaultAttribs (vtg "name") (read $ vtg "arity")
                  where vtg t = verbatim $ tag "funcsym" /> tag t /> txt $ c

parseStrategy :: MonadError ParseError m => Content -> m Strategy
parseStrategy doc = case verbatim $ tag "problem" /> tag "strategy" /> txt $ doc of
                      "INNERMOST" -> return Innermost
                      "OUTERMOST" -> return Outermost
                      "FULL" -> return Full
                      "NONE" -> return Full
                      a -> throwError $ UnsupportedStrategy a

parseStartTerms :: MonadError ParseError m => Set F.Symbol -> Content -> m StartTerms
parseStartTerms defs doc = case tag "problem" /> tag "startterm" /> txt $ doc of
                            [] -> return TermAlgebra
                            _ -> return $ BasicTerms defs
                               


newtype TrsParser a = P { 
    runParser :: ErrorT ParseError (RWS SymMap () V.Variables) a
  } deriving (Monad, MonadError ParseError)



liftP ::  RWS SymMap () V.Variables a -> TrsParser a
liftP m = P $ lift m

sym :: String -> TrsParser F.Symbol
sym name = do ms <- liftP $ ask >>= return . Map.lookup name
              case ms of 
                Just s -> return s
                Nothing  -> throwError $ SymbolNotInSignature name

var :: String -> TrsParser V.Variable
var name = liftP $ do vars <- get
                      case V.variable name vars of 
                        Just some  -> return some
                        Nothing -> put vars' >> return fresh
                                  where (fresh, vars') = V.fresh name $ vars


parseTrs :: MonadError ParseError m => F.Signature -> SymMap ->  Content -> m (Trs.Trs,Trs.Trs)
parseTrs sig syms doc = case fst $ evalRWS run syms V.emptyVariables of
                          Left e  -> throwError e
                          Right p -> return p
  where run = runErrorT $ runParser $ parseRules sig doc


parseRules :: F.Signature -> Content -> TrsParser (Trs.Trs,Trs.Trs)
parseRules sig doc = do strict <- mapM parseRule $ tag "rules" /> tag "rule" $ doc
                        weak <- mapM parseRule $ tag "rules" /> tag "relrules" /> tag "rule" $ doc
                        vars <- liftP get
                        return (Trs.makeTrs strict sig vars, Trs.makeTrs weak sig vars)       

parseRule :: Content -> TrsParser R.Rule
parseRule doc = liftM2 R.Rule parseLhs parseRhs
  where parseLhs,parseRhs :: TrsParser Term
        parseLhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "lhs") $ doc
        parseRhs = parseOne err parseTerm $ cElems `o` (tag "rule" /> tag "rhs") $ doc
        err = throwError $ MalformedRule doc

parseTerm :: Content -> TrsParser Term
parseTerm doc = maybe err id (pv `mplus` pt)
  where pt = case verbatim $ tag "funapp" /> tag "name" /> txt $ doc of 
               "" -> Nothing
               s  -> Just $ liftM2 Fun (sym s) (forM args parseTerm)
                 where args = cElems `o` (tag "funapp" /> tag "arg") $ doc
        pv = case tag "var" $ doc of 
               [v] -> Just $  (var $ verbatim $ tag "var" /> txt $ v) >>= return . Var
               _ -> Nothing
        err    = throwError $ MalformedTerm doc



  