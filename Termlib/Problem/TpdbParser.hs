{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Termlib.Problem.TpdbParser where

import Control.Monad (liftM)
import Control.Monad.Error
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec.Char
import Termlib.Problem
import Text.Parsec hiding (ParseError)
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Rule as R
import qualified Termlib.Term as Term
import qualified Termlib.Trs as T
import Termlib.Utils (PrettyPrintable(..))
import Termlib.Problem.Parser
import Control.Monad.Writer.Lazy

type TPDBParser a = ParsecT String Problem (ErrorT ParseError (Writer [ParseWarning])) a 

warn :: ParseWarning -> TPDBParser ()
warn a = lift $ tell [a]

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString input = case runWriter $ runErrorT $ runParserT parseProblem stdprob "trs input" input of 
                            (Left e,             _    ) -> Left e
                            (Right (Left e),     _    ) -> Left $ ParsecParseError e
                            (Right (Right prob), warns) -> Right (prob, warns)
                          where stdprob = standardProblem TermAlgebra Full T.empty

parseProblem :: TPDBParser Problem
parseProblem = speclist >> getState

getTrs :: TPDBParser T.Trs
getTrs = strictTrs `liftM` getState

setTrs :: T.Trs -> TPDBParser ()
setTrs trs = do prob <- getState
                setState $ prob{relation = Standard trs}
                return ()

setStartTerms :: StartTerms -> TPDBParser ()
setStartTerms st = do prob <- getState
                      setState $ prob{startTerms = st}
                      return ()

setStrategy :: Strategy -> TPDBParser ()
setStrategy strat = do prob <- getState
                       setState $ prob{strategy = strat}
                       return ()

onTrs :: T.TrsMonad a -> TPDBParser a
onTrs m = do trs <- getTrs
             let (a,trs') = T.runTrs m trs
             setTrs trs'
             return a

addFreshVar :: String -> TPDBParser ()
addFreshVar name = onTrs (T.getVariable name) >> return ()

addRule :: R.Rule -> TPDBParser ()
addRule r = onTrs (T.addRule r) >> return ()

speclist :: TPDBParser ()
speclist = many spec >> eof >> return ()

spec = do finwhite (char '(')
          spec'
          finwhite (char ')')
          return ()

spec' = (string "VAR" >> whitespaces >> varlist <?> "VAR declaration")
        <|> (string "RULES" >> whitespaces >> listofrules <?> "RULES declaration")
        <|> (string "THEORY" >> whitespaces >> listofthdecl <?> "THEORY declaration")
        <|> (try (string "STRATEGY") >> whitespaces >> strategydecl <?> "STRATEGY declaration")
        <|> (try (string "STARTTERM") >> whitespaces >> starttermdecl <?> "STARTTERM declaration")
        <|> (string "PROOF" >> whitespaces >> typeofproof <?> "PROOF declaration")
--        <|> (ident >> whitespaces >> anylist <?> "any declaration")

varlist :: TPDBParser ()
varlist = idlist >>= mapM_ addFreshVar

listofrules = many (inwhite rule) >>= mapM_ addRule

rule = do lhs <- term
          whitespaces
          sep <- try (string "->=") <|> string "->"
          whitespaces
          rhs <- term
          condlist <- try (whitespaces >> char '|' >> whitespaces >> condlist) <|> return []
          if sep == "->=" then error "Relative Termination not supported" else
            if null condlist then return (R.Rule lhs rhs) else
            error "Conditional Rewriting not supported"

term = try complexterm <|> simpleterm

complexterm = do name <- ident
                 finwhite $ char '('
                 subterms <- termlist
                 whitespaces
                 char ')'
                 onTrs $ flip Term.Fun subterms `liftM` (T.getSymbol (F.defaultAttribs name (length subterms)))

simpleterm = do name <- ident
                onTrs (do isVar <- T.isVariable name
                          case isVar of
                            True  -> Term.Var `liftM` T.getVariable name
                            False -> flip Term.Fun [] `liftM` (T.getSymbol (F.defaultAttribs name 0)))

termlist = sepBy term (inwhite (char ','))

condlist = sepBy cond (inwhite (char ','))

cond = do term
          whitespaces
          try (string "-><-") <|> string "->"
          finwhite term
          return $ error "Conditional Rewriting not supported"

listofthdecl = many (whitespaces >> char '(' >> finwhite thdecl >> char ')') >> return ()

thdecl = (try theq >> error "Theory declarations not supported")
         <|> (thid >> error "Theory declarations not supported")

theq = string "EQUATIONS" >> eqlist

thid = ident >> whitespaces >> idlist

eqlist = many (inwhite equation)

equation = do term
              whitespaces
              string "=="
              whitespaces
              term
              return $ error "Theory declarations not supported"

strategydecl = try sfull <|> try sinner <|> try souter <|> scons

sfull = string "FULL" >> setStrategy Full

sinner = string "INNERMOST" >> setStrategy Innermost

souter = string "OUTERMOST" >> error "Outermost strategy not supported"

scons = string "CONTEXTSENSITIVE" >> csstratlist >> error "Context-Sensitive Rewriting not supported"

csstratlist = many (inwhite csstrat)

csstrat = do char '('
             finwhite ident
             intlist
             whitespaces
             char ')'
             return $ error "Context-Sensitive Rewriting not supported"

intlist = many (inwhite oneint)

oneint = do is <- many1 digit
            return (read is :: Int)

starttermdecl = try sta <|> try scb <|> sautomat

sta = string "FULL" >> setStartTerms TermAlgebra

scb = string "CONSTRUCTOR-BASED" >> setStartTerms (BasicTerms Set.empty)

sautomat = string "AUTOMATON" >> automatonstuff >> error "Automaton specified start term sets not supported"

automatonstuff = anylist

typeofproof = (string "TERMINATION" <|> string "COMPLEXITY") >> return ()

ident = many1 (try innocentmin <|> try innocenteq <|> noneOf " \n\r\t()\",|-=")
        where innocentmin = do char '-'
                               notFollowedBy $ char '>'
                               return '-'
              innocenteq  = do char '='
                               notFollowedBy $ char '='
                               return '='

anylist = (anylist1 <|> anylist2 <|> anylist3 <|> anylist4 <|> anylist5) >> return ()

anylist1 = do char '('
              finwhite anylist
              char ')'
              whitespaces
              anylist
              return ()

anylist2 = do char ','
              whitespaces
              anylist
              return ()

anylist3 = do ident
              whitespaces
              anylist
              return ()

anylist4 = do astring
              whitespaces
              anylist
              return ()

anylist5 = return ()

astring = do char '"'
             res <- many (noneOf "\"")
             char '"'
             return res

idlist = many (inwhite ident)

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaces = many whitespace

whitespaces1 = many1 whitespace

inwhite f = try (do whitespaces
                    res <- f
                    whitespaces
                    return res)

finwhite f = do whitespaces
                res <- f
                whitespaces
                return res
