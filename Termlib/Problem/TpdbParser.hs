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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Termlib.Problem.TpdbParser where

import Control.Monad.Error
import Text.ParserCombinators.Parsec.Char
import Termlib.Problem
import qualified Termlib.ContextSensitive as CS
import Text.Parsec hiding (ParseError)
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Rule as R
import qualified Termlib.Term as Term
import qualified Termlib.Variable as V
import qualified Termlib.Trs as T
import Termlib.Problem.ParseErrors (ParseError (..), ParseWarning (..))
import Control.Monad.Writer.Lazy
import qualified Termlib.Signature as Signature
import Termlib.Variable (Variables)


type TPDBParser a = ParsecT String Problem (ErrorT ParseError (Writer [ParseWarning])) a 

warn :: ParseWarning -> TPDBParser ()
warn a = lift $ tell [a]

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString input = case runWriter $ runErrorT $ runParserT parseProblem stdprob ".trs problem input" input of 
                            (Left e,             _    ) -> Left e
                            (Right (Left e),     _    ) -> Left $ ParsecParseError e
                            (Right (Right prob), warns) -> Right (prob{startTerms = finStartTerms prob}, warns)
    where stdprob = Problem { startTerms = TermAlgebra
                            , strategy   = Full
                            , variables  = V.emptyVariables
                            , signature  = F.emptySignature
                            , strictDPs  = T.empty 
                            , strictTRS  = T.empty
                            , weakDPs    = T.empty
                            , weakTRS    = T.empty} 
          finStartTerms prob = mkStartTerms (startTerms prob) (T.definedSymbols rs) (T.constructors rs)
              where rs = allRules prob
          mkStartTerms TermAlgebra _ _ = TermAlgebra
          mkStartTerms (BasicTerms _ _) d c = BasicTerms d c 

parseProblem :: TPDBParser Problem
parseProblem = speclist >> getState

modifyProblem :: (Problem -> Problem) -> TPDBParser ()
modifyProblem f = getState >>= (putState . f)

modifyStrictTrs :: (T.Trs -> T.Trs) -> TPDBParser ()
modifyStrictTrs f = modifyProblem f'
  where f' prob = prob { strictTRS = f $ strictTRS prob }

modifyWeakTrs :: (T.Trs -> T.Trs) -> TPDBParser ()
modifyWeakTrs f = modifyProblem f'
  where f' prob = prob { weakTRS = f $ weakTRS prob }

setStartTerms :: StartTerms -> TPDBParser ()
setStartTerms st = do prob <- getState
                      setState $ prob{startTerms = st}
                      return ()

setStrategy :: Strategy -> TPDBParser ()
setStrategy strat = do prob <- getState
                       setState $ prob{strategy = strat}
                       return ()

onSignature :: F.SignatureMonad a -> TPDBParser a
onSignature m = do prob <- getState
                   let (a,sig') = Signature.runSignature m $ signature prob
                   putState $ prob {signature = sig'}
                   return a

getSymbol :: F.FunctionName -> F.Arity -> TPDBParser F.Symbol
getSymbol name arity = onSignature m 
    where m = do sig <- Signature.getSignature 
                 let setarity (Just p) = Just (p {F.symArity = arity})
                 case F.symbol name sig of 
                   Just sym -> do Signature.modifySignature $ Signature.alterAttributes setarity sym
                                  return sym
                   Nothing  -> F.fresh (F.defaultAttribs name arity)

getVariables :: TPDBParser Variables 
getVariables = variables `liftM` getState 

onVariables :: V.VariableMonad a -> TPDBParser a
onVariables m = do prob <- getState
                   let (a,vars') = Signature.runSignature m $ variables prob
                   putState $ prob {variables = vars'}
                   return a


addFreshVar :: String -> TPDBParser ()
addFreshVar name = onVariables (V.maybeFresh name) >> return ()

addStrictRule :: R.Rule -> TPDBParser ()
addStrictRule r = modifyStrictTrs (T.insert r) >> return ()

addWeakRule :: R.Rule -> TPDBParser ()
addWeakRule r = modifyWeakTrs (T.insert r) >> return ()

speclist :: TPDBParser ()
speclist = many spec >> eof >> return ()

spec :: TPDBParser ()
spec = do _ <- finwhite (char '(')
          _ <- spec'
          _ <- finwhite (char ')')
          return ()
    where spec' = (string "VAR" >> whitespaces >> varlist <?> "VAR declaration")
              <|> (string "RULES" >> whitespaces >> listofrules <?> "RULES declaration")
              <|> (string "THEORY" >> whitespaces >> listofthdecl <?> "THEORY declaration")
              <|> (try (string "STRATEGY") >> whitespaces >> strategydecl <?> "STRATEGY declaration")
              <|> (try (string "STARTTERM") >> whitespaces >> starttermdecl <?> "STARTTERM declaration")
              <|> (string "PROOF" >> whitespaces >> typeofproof <?> "PROOF declaration")
              <|> (ident >> whitespaces >> anylist <?> "any declaration")

varlist :: TPDBParser ()
varlist = idlist >>= mapM_ addFreshVar


listofrules :: TPDBParser ()
listofrules = many (inwhite rule) >> return ()


rule :: TPDBParser ()
rule = do lhs   <- term
          _     <- whitespaces
          sep   <- try (string "->=") <|> string "->"
          _     <- whitespaces
          rhs   <- term
          clist <- try (whitespaces >> char '|' >> whitespaces >> condlist) <|> return []
          if null clist then (if sep == "->=" then addWeakRule else addStrictRule) (R.Rule lhs rhs)  else 
            throwError $ UnsupportedRewritingError "Conditional rewriting"

term :: TPDBParser Term.Term
term = try complexterm <|> simpleterm

complexterm :: TPDBParser Term.Term
complexterm = do name     <- ident
                 _        <- finwhite $ char '('
                 subterms <- termlist
                 _        <- whitespaces
                 _        <- char ')'
                 sym      <- getSymbol name (length subterms)
                 return $ Term.Fun sym subterms

simpleterm :: TPDBParser Term.Term
simpleterm = do name <- ident
                vars <- getVariables
                case V.variable name vars of 
                  Just v -> return $ Term.Var v
                  Nothing -> do sym <- getSymbol name 0
                                return $ Term.Fun sym []

termlist :: TPDBParser [Term.Term]
termlist = sepBy term (inwhite (char ','))

condlist :: TPDBParser [()]
condlist = sepBy cond (inwhite (char ','))

cond :: TPDBParser ()
cond = do _ <- term
          _ <- whitespaces
          _ <- try (string "-><-") <|> string "->"
          _ <- finwhite term
          throwError $ UnsupportedRewritingError "Conditional rewriting"

listofthdecl :: TPDBParser ()
listofthdecl = many (whitespaces >> char '(' >> finwhite thdecl >> char ')') >> return ()

thdecl :: TPDBParser ()
thdecl = (try theq >> throwError (UnsupportedRewritingError "Theory declarations"))
         <|> (thid >> throwError (UnsupportedRewritingError "Theory declarations"))

theq :: TPDBParser [()]
theq = string "EQUATIONS" >> eqlist

thid :: TPDBParser [String]
thid = ident >> whitespaces >> idlist

eqlist :: TPDBParser [()]
eqlist = many (inwhite equation)

equation :: TPDBParser ()
equation = do _ <- term
              _ <- whitespaces
              _ <- string "=="
              _ <- whitespaces
              _ <- term
              return ()

strategydecl :: TPDBParser ()
strategydecl = try sfull <|> try sinner <|> try souter <|> scons

sfull :: TPDBParser ()
sfull = string "FULL" >> setStrategy Full

sinner :: TPDBParser ()
sinner = string "INNERMOST" >> setStrategy Innermost

souter :: TPDBParser ()
souter = string "OUTERMOST" >> setStrategy Outermost

scons :: TPDBParser ()
scons = do string "CONTEXTSENSITIVE" 
           rmap <- csstratlist 
           setStrategy (ContextSensitive rmap)

csstratlist :: TPDBParser ReplacementMap
csstratlist = do ls <- many (inwhite csstrat)
                 return $ foldl (\ rm (sym, is) -> CS.setReplacing sym is rm) CS.empty ls

csstrat :: TPDBParser (F.Symbol, [Int])
csstrat = do _ <- char '('
             name <- finwhite ident
             is <- intlist
             _ <- whitespaces
             _ <- char ')'
             sym <- getSymbol name 0
             return (sym, is)

intlist :: TPDBParser [Int]
intlist = many (inwhite oneint)

oneint :: TPDBParser Int
oneint = do is <- many1 digit
            return (read is :: Int)

starttermdecl :: TPDBParser ()
starttermdecl = try sta <|> try scb <|> sautomat

sta :: TPDBParser ()
sta = string "FULL" >> setStartTerms TermAlgebra

scb :: TPDBParser ()
scb = string "CONSTRUCTOR-BASED" >> setStartTerms (BasicTerms undefined undefined)

sautomat :: TPDBParser ()
sautomat = string "AUTOMATON" >> automatonstuff >> warn (PartiallySupportedStartTerms "Automaton specified start term set") >> setStartTerms TermAlgebra

automatonstuff :: TPDBParser ()
automatonstuff = anylist

typeofproof :: TPDBParser ()
typeofproof = (string "TERMINATION" <|> string "COMPLEXITY") >> return ()

ident :: TPDBParser String
ident = many1 (try innocentmin <|> try innocenteq <|> noneOf " \n\r\t()\",|-=")
        where innocentmin = do _ <- char '-'
                               _ <- notFollowedBy $ char '>'
                               return '-'
              innocenteq  = do _ <- char '='
                               _ <- notFollowedBy $ char '='
                               return '='

laxident :: TPDBParser String
laxident = many1 (noneOf " \n\r\t()\",|")

anylist :: TPDBParser ()
anylist = (try anylist1 <|> try anylist2 <|> try anylist3 <|> try anylist4 <|> anylist5) >> return ()

anylist1 :: TPDBParser ()
anylist1 = do _ <- char '('
              _ <- finwhite anylist
              _ <- char ')'
              _ <- whitespaces
              _ <- anylist
              return ()

anylist2 :: TPDBParser ()
anylist2 = do _ <- char ','
              _ <- whitespaces
              _ <- anylist
              return ()

anylist3 :: TPDBParser ()
anylist3 = do _ <- laxident
              _ <- whitespaces
              _ <- anylist
              return ()

anylist4 :: TPDBParser ()
anylist4 = do _ <- astring
              _ <- whitespaces
              _ <- anylist
              return ()

anylist5 :: TPDBParser ()
anylist5 = return ()

astring :: TPDBParser String
astring = do _   <- char '"'
             res <- many (noneOf "\"")
             _   <- char '"'
             return res

idlist :: TPDBParser [String]
idlist = many (inwhite ident)

whitespace :: TPDBParser Char
whitespace = space <|> newline <|> tab <|> char '\r'

whitespaces :: TPDBParser String
whitespaces = many whitespace

whitespaces1 :: TPDBParser String
whitespaces1 = many1 whitespace

inwhite :: TPDBParser a -> TPDBParser a
inwhite f = try (do _   <-whitespaces
                    res <- f
                    _   <- whitespaces
                    return res)

finwhite :: TPDBParser a -> TPDBParser a
finwhite f = do _   <- whitespaces
                res <- f
                _   <- whitespaces
                return res
