{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Termlib.Problem.TpdbParser where

import Control.Monad (liftM)
import Control.Monad.Error
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

type TPDBParser a = ParsecT String T.Trs (ErrorT ParseError (Writer [ParseWarning])) a 

warn :: ParseWarning -> TPDBParser ()
warn a = lift $ tell [a]

problemFromString :: String -> Either ParseError (Problem,[ParseWarning])
problemFromString input = case runWriter $ runErrorT $ runParserT parseProblem T.empty "trs input" input of 
                            (Left e,             _    ) -> Left e
                            (Right (Left e),     _    ) -> Left $ ParsecParseError e
                            (Right (Right prob), warns) -> Right (prob, warns)

parseProblem :: TPDBParser Problem
parseProblem = undefined

getTrs :: TPDBParser T.Trs
getTrs = getState

setTrs :: T.Trs -> TPDBParser ()
setTrs = setState

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

termlist = undefined

condlist = undefined

listofthdecl = undefined

strategydecl = undefined

starttermdecl = undefined

typeofproof = undefined

ident = many (try innocentmin <|> try innocenteq <|> (noneOf " \n\r\t()\",|-="))
        where innocentmin = do char '-'
                               notFollowedBy $ char '>'
                               return '-'
              innocenteq  = do char '='
                               notFollowedBy $ char '='
                               return '='

anylist = undefined

idlist = sepBy ident whitespaces

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaces = many whitespace
