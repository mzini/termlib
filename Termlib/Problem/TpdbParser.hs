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
speclist = endBy spec eof >> return ()

spec = do char '('
          whitespaces
          spec'
          whitespaces
          char ')'
          return ()


spec' = (string "VAR" >> whitespaces >> varlist)
        <|> (string "RULES" >> whitespaces >> listofrules)
        <|> (string "THEORY" >> whitespaces >> listofthdecl)
        <|> (try (string "STRATEGY") >> whitespaces >> strategydecl)
        <|> (try (string "STARTTERM") >> whitespaces >> starttermdecl)
        <|> (string "PROOF" >> whitespaces >> typeofproof)
        <|> (ident >> whitespaces >> anylist)

varlist :: TPDBParser ()
varlist = idlist >>= mapM_ addFreshVar

listofrules = sepBy rule whitespaces >>= mapM_ addRule

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
                 whitespaces
                 char '('
                 whitespaces
                 subterms <- termlist
                 whitespaces
                 char ')'
                 onTrs $ flip Term.Fun subterms `liftM` (T.getSymbol (F.defaultAttribs name (length subterms)))

simpleterm = do name <- ident
                onTrs (do isVar <- T.isVariable name
                          case isVar of
                            True  -> Term.Var `liftM` T.getVariable name
                            False -> flip Term.Fun [] `liftM` (T.getSymbol (F.defaultAttribs name 0)))

termlist = sepBy term (whitespaces >> char ',' >> whitespaces)

condlist = sepBy cond (whitespaces >> char ',' >> whitespaces)

cond = do term
          whitespaces
          try (string "-><-") <|> string "->"
          whitespaces
          term
          return $ error "Conditional Rewriting not supported"

listofthdecl = sepBy (char '(' >> whitespaces >> thdecl >> whitespaces >> char ')') whitespaces >> return ()

thdecl = (try theq >> error "Theory declarations not supported")
         <|> (thid >> error "Theory declarations not supported")

theq = string "EQUATIONS" >> eqlist

thid = ident >> whitespaces >> idlist

eqlist = sepBy equation whitespaces

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

csstratlist = sepBy csstrat whitespaces

csstrat = do char '('
             whitespaces
             ident
             whitespaces
             intlist
             whitespaces
             char ')'
             return $ error "Context-Sensitive Rewriting not supported"

intlist = sepBy oneint whitespaces

oneint = do i <- digit
            is <- many digit
            return (read (i:is) :: Int)

starttermdecl = try sta <|> try scb <|> sautomat

sta = string "FULL" >> setStartTerms TermAlgebra

scb = string "CONSTRUCTOR-BASED" >> setStartTerms (BasicTerms Set.empty)

sautomat = string "AUTOMATON" >> automatonstuff >> error "Automaton specified start term sets not supported"

automatonstuff = anylist

typeofproof = (string "TERMINATION" <|> string "COMPLEXITY") >> return ()

ident = many (try innocentmin <|> try innocenteq <|> (noneOf " \n\r\t()\",|-="))
        where innocentmin = do char '-'
                               notFollowedBy $ char '>'
                               return '-'
              innocenteq  = do char '='
                               notFollowedBy $ char '='
                               return '='

anylist = (anylist1 <|> anylist2 <|> anylist3 <|> anylist4 <|> anylist5) >> return ()

anylist1 = do char '('
              whitespaces
              anylist
              whitespaces
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

idlist = sepBy ident whitespaces

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaces = many whitespace
