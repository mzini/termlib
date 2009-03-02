{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Trs.XmlParser where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Control.Monad.Error
import Control.Monad (liftM, forM)
import Control.Monad.Trans (lift)
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Control.Monad.State

import qualified Termlib.Rule as R
import qualified Termlib.Term as T
import qualified Termlib.Trs as Trs
import qualified Termlib.Variable as V
import qualified Termlib.FunctionSymbol as F
import Termlib.Trs


data ParseError = MalformedTerm Content
                | MalformedRule Content
                | UnknownError String
                | SymbolNotInSignature String

instance Show ParseError where
  show (MalformedTerm s) = "MalformedTerm:\n" ++ verbatim s
  show (MalformedRule s) = "MalformedRule:\n" ++ verbatim s
  show (SymbolNotInSignature sym) = "SymbolNotInSignature " ++ sym
  show (UnknownError e) = e

instance Error ParseError where
  strMsg = UnknownError

type SymMap = Map.Map String F.Symbol
newtype Parser a = P { 
    runParser :: ErrorT ParseError (RWS SymMap () V.Variables) a
  } deriving (Monad, MonadError ParseError)

liftP ::  RWS SymMap () V.Variables a -> Parser a
liftP m = P $ lift m

sym :: String -> Parser F.Symbol
sym name = do ms <- liftP $ ask >>= return . Map.lookup name
              case ms of 
                Just s -> return s
                Nothing  -> throwError $ SymbolNotInSignature name

var :: String -> Parser V.Variable
var name = liftP $ do vars <- get
                      case V.variable name vars of 
                        Just some  -> return some
                        Nothing -> put vars' >> return fresh
                                  where (fresh, vars') = V.fresh name $ vars


trssFromXml :: F.Signature -> SymMap ->  Content -> Either ParseError (Trs.Trs,Trs.Trs)
trssFromXml sig syms doc = fst $ evalRWS run syms V.emptyVariables 
  where run = runErrorT $ runParser $ parseRules sig doc

parseOne _ p [a] = p a
parseOne e _ _   = e

parseRules :: F.Signature -> Content -> Parser (Trs.Trs,Trs.Trs)
parseRules sig doc = do strict <- mapM parseRule $ tag "rules" /> tag "rule" $ doc
                        weak <- mapM parseRule $ tag "rules" /> tag "relrules" /> tag "rule" $ doc
                        vars <- liftP get
                        return (Trs.makeTrs strict sig vars, Trs.makeTrs weak sig vars)       

cElems a@(CElem _) = keep a
cElems a           = none a

parseRule :: Content -> Parser R.Rule
parseRule doc = liftM2 R.Rule parseLhs parseRhs
  where parseLhs,parseRhs :: Parser T.Term
        parseLhs = parseOne err parseTerm $ cElems `o` children `o` (tag "rule" /> tag "lhs") $ doc
        parseRhs = parseOne err parseTerm $ cElems `o` children `o` (tag "rule" /> tag "rhs") $ doc
        err = throwError $ MalformedRule doc

parseTerm :: Content -> Parser T.Term
parseTerm doc = maybe err id (pv `mplus` pt)
  where pt = case verbatim $ tag "funapp" /> tag "name" /> txt $ doc of 
               "" -> Nothing
               s  -> Just $ liftM2 T.Fun (sym s) (forM args parseTerm)
                 where args = cElems `o` children `o` (tag "funapp" /> tag "arg") $ doc
        pv = case tag "var" $ doc of 
               [v] -> Just $  (var $ verbatim $ tag "var" /> txt $ v) >>= return . T.Var
               _ -> Nothing
        err    = throwError $ MalformedTerm doc
              

                       