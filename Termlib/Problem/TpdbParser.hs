module Termlib.Problem.TpdbParser where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
import Termlib.Problem
import qualified Termlib.Trs as T

type TPDBParser a = CharParser T.Trs a

--getTrs :: TPDBParser T.Trs
--getTrs = getState
onTrs :: T.TrsMonad a -> TPDBParser a
onTrs m = do trs <- getState
             let (a,trs') = T.runTrs m trs
             setState trs'
             return a

addFreshVar :: String -> TPDBParser ()
addFreshVar name = onTrs (T.getVariable name) >> return ()

speclist :: TPDBParser ()
speclist = endBy spec eof >> return ()

spec = do char '('
          spec'
          char ')'
          return ()

spec' = (string "VAR" >> varlist)
        <|> (string "RULES" >> listofrules)
        <|> (string "THEORY" >> listofthdecl)
        <|> (try (string "STRATEGY") >> strategydecl)
        <|> (try (string "STARTTERM") >> starttermdecl)
        <|> (string "PROOF" >> typeofproof)
        <|> (ident >> anylist)

varlist = idlist >>= mapM_ addFreshVar

listofrules = undefined

listofthdecl = undefined

strategydecl = undefined

starttermdecl = undefined

typeofproof = undefined

ident = many (noneOf " \n\r\t()\",|")

anylist = undefined

idlist = sepBy ident whitespaces

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaces = many whitespace
