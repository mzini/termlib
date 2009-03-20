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
import qualified Termlib.Variable as V
import qualified Termlib.Trs as T
import Termlib.Utils (PrettyPrintable(..))
import Termlib.Problem.Parser
import Control.Monad.Writer.Lazy
import qualified Termlib.Signature as Signature
import Termlib.Signature (SignatureMonad)
import Termlib.FunctionSymbol (Symbol, Signature)
import Termlib.Variable (Variable, Variables)
type TPDBParser a = ParsecT String Problem (ErrorT ParseError (Writer [ParseWarning])) a 

warn :: ParseWarning -> TPDBParser ()
warn a = lift $ tell [a]

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString input = case runWriter $ runErrorT $ runParserT parseProblem stdprob "trs input" input of 
                            (Left e,             _    ) -> Left e
                            (Right (Left e),     _    ) -> Left $ ParsecParseError e
                            (Right (Right prob), warns) -> Right (prob, warns)
                          where stdprob = standardProblem TermAlgebra Full T.empty V.emptyVariables F.emptySignature

parseProblem :: TPDBParser Problem
parseProblem = speclist >> getState


modifyRelation :: (Relation -> Relation) -> TPDBParser ()
modifyRelation f = do prob <- getState
                      putState $ prob {relation = f $ relation prob}

modifyStrictTrs :: (T.Trs -> T.Trs) -> TPDBParser ()
modifyStrictTrs f = modifyRelation f'
  where f' (Standard trs) = Standard $ f trs
        f' (Relative strict weak) = Relative (f strict) weak

modifyWeakTrs :: (T.Trs -> T.Trs) -> TPDBParser ()
modifyWeakTrs f = modifyRelation f'
  where f' (Standard trs) = Relative trs $ f T.empty
        f' (Relative strict weak) = Relative strict (f weak)

setStartTerms :: StartTerms -> TPDBParser ()
setStartTerms st = do prob <- getState
                      setState $ prob{startTerms = st}
                      return ()

setStrategy :: Strategy -> TPDBParser ()
setStrategy strat = do prob <- getState
                       setState $ prob{strategy = strat}
                       return ()

onSignature :: SignatureMonad Symbol F.Attributes a -> TPDBParser a
onSignature m = do prob <- getState
                   let (a,sig') = Signature.runSignature m $ signature prob
                   putState $ prob {signature = sig'}
                   return a

getVariables :: TPDBParser Variables 
getVariables = variables `liftM` getState 

onVariables :: SignatureMonad Variable V.Attributes a -> TPDBParser a
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
spec = do finwhite (char '(')
          spec'
          finwhite (char ')')
          return ()

spec' :: TPDBParser ()
spec' = (string "VAR" >> whitespaces >> varlist <?> "VAR declaration")
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
rule = do lhs <- term
          whitespaces
          sep <- try (string "->=") <|> string "->"
          whitespaces
          rhs <- term
          condlist <- try (whitespaces >> char '|' >> whitespaces >> condlist) <|> return []
          if null condlist then (if sep == "->=" then addWeakRule else addStrictRule) (R.Rule lhs rhs)  else 
            error "Conditional Rewriting not supported"

term :: TPDBParser Term.Term
term = try complexterm <|> simpleterm

complexterm :: TPDBParser Term.Term
complexterm = do name <- ident
                 finwhite $ char '('
                 subterms <- termlist
                 whitespaces
                 char ')'
                 onSignature $ flip Term.Fun subterms `liftM` (Signature.maybeFresh (F.defaultAttribs name (length subterms)))

simpleterm :: TPDBParser Term.Term
simpleterm = do name <- ident
                vars <- getVariables
                case V.variable name vars of 
                  Just v -> return $ Term.Var v
                  Nothing -> onSignature $ flip Term.Fun [] `liftM` (F.maybeFresh (F.defaultAttribs name 0))

termlist :: TPDBParser [Term.Term]
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
