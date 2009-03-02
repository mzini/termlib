{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Problem.XmlParser where

import Control.Monad.Error

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import Termlib.Signature.XmlParser
import Termlib.Trs.XmlParser
import Termlib.Problem
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Trs as Trs


problemFromString str = parseProblem doc
  where doc = getContent $ xmlParse "xml:signature" str  
        getContent (Document _ _ e _) = CElem e

parseProblem :: Content -> Either ParseError Problem
parseProblem doc = do (sig,symMap) <- case map signatureFromXml $ tag "problem" /> tag "trs" /> tag "signature" $ doc of 
                                       [a] -> return a
                                       _   -> throwError $ UnknownError "Error when parsing signature"
                      l <- mapM (trssFromXml sig symMap) $ tag "problem" /> tag "trs" /> tag "rules" $ doc
                      (strict, weak) <- case l of 
                        [a] -> return a
                        _   -> throwError $ UnknownError "Error when parsing trs"
                      strategy <- parseStrategy doc
                      startTerms <- parseStartTerms undefined doc -- TODO
                      case Trs.isEmpty weak of  
                        True -> return $ standardProblem startTerms strategy strict
                        False -> return $ relativeProblem startTerms strategy strict weak
                            


parseStrategy :: MonadError ParseError m => Content -> m Strategy
parseStrategy doc = case verbatim $ tag "problem" /> tag "strategy" /> txt $ doc of
                      "INNERMOST" -> return Innermost
                      "OUTERMOST" -> return Outermost
                      "FULL" -> return Full
                      "NONE" -> return Full

parseStartTerms :: MonadError ParseError m => F.Signature -> Content -> m StartTerms
parseStartTerms sig doc = case tag "problem" /> tag "startterm" /> txt $ doc of
                            [] -> return TermAlgebra
                            _ -> return TermAlgebra -- TODO finish
                               


  