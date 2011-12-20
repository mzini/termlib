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
module Termlib.Term.Parser where
import Data.List (isSuffixOf)
import Termlib.FunctionSymbol (Signature, Symbol)
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Signature as Sig
import Termlib.Variable (Variables, Variable(..))
import qualified Termlib.Variable as V
import Termlib.Problem.ParseErrors(ParseError(..), ParseWarning(..))
import Termlib.Term (Term(..), root, immediateSubterms)
import Termlib.Rule (Rule(..))
import Control.Monad.Error
import Control.Monad.Writer.Lazy
import Text.Parsec hiding (ParseError)

parseFromString :: Signature -> Variables -> TermParser a -> String -> Either ParseError ((a,Signature,Variables),[ParseWarning])
parseFromString sig vars parser input = 
  case runWriter $ runErrorT $ runParserT term' (sig,vars) input input of 
    (Left e,             _    ) -> Left e
    (Right (Left e),     _    ) -> Left $ ParsecParseError e
    (Right (Right t), warns) -> Right (t, warns)
    where term' = 
            do e <- parser
               (fs,vs) <- getState
               return (e,fs,vs)

type TermParser a = ParsecT String (Signature,Variables) (ErrorT ParseError (Writer [ParseWarning])) a

rule :: TermParser (Bool,Rule)
rule = 
  do l <- term
     str <- whitespaced (try weak <|> strict)
     r <- term
     return $ (str,Rule l r)
    where strict = string "->" >> return True
          weak = string "->=" >> return False
     
-- dpRule :: TermParser (Bool, Rule)
-- dpRule = 
--   do (s,rl) <- rule
--      setMarked (root (lhs rl))
--      setCompound (root (rhs rl))
--      mapM_ (setMarked . root) (immediateSubterms (rhs rl))
--      return (s,rl)
--     where modifyAttrib _     (Left _) = return ()
--           modifyAttrib alter (Right f) = 
--             do (fs,vs) <- getState
--                putState (Sig.alterAttributes (fmap alter) f fs, vs)
--           setCompound = modifyAttrib (\ attrib -> attrib { F.symIsCompound = True})
--           setMarked = modifyAttrib (\ attrib -> attrib { F.symIsMarked = True})

term :: TermParser Term
term = 
  do name <- ident
     try (parseFun name) <|> parseVar name
  where parseFun name = do ts <- parens $ sepBy (whitespaced term) colon
                           f <- getSym name (length ts)
                           return $ Fun f ts
        parseVar name = Var `liftM` getVar name
  
getVar :: String -> TermParser Variable
getVar name = do (fs,vs) <- getState
                 let (v, vs') = Sig.runSignature (V.maybeFresh name) vs
                 putState (fs,vs')
                 return v 

getSym :: String -> Int -> TermParser Symbol
getSym name ar = 
  do (fs,vs) <- getState
     let (f, fs') = Sig.runSignature (F.maybeFresh attribs) fs
     putState (fs',vs)
     return f
  where attribs | name == "COM"           = (F.defaultAttribs name ar) {F.symIsCompound = True}
                | "^#" `isSuffixOf` name = (F.defaultAttribs (dropTl 2 name) ar) {F.symIsMarked = True}
                | "#" `isSuffixOf` name  = (F.defaultAttribs (dropTl 1 name) ar) {F.symIsMarked = True}
                | otherwise              = F.defaultAttribs name ar
        dropTl i s = take (length s - i) s
        
colon = char ','

ident = many1 $ noneOf " \n\r\t()\",|-= "

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaced p = do many $ whitespace
                   f <- p
                   many $ whitespace
                   return f

parens p = do char '('
              e <- p
              char ')'
              return e