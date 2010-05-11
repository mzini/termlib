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
import Termlib.FunctionSymbol (Signature, Symbol)
import qualified Termlib.FunctionSymbol as F
import Termlib.Variable (Variables, Variable(..))
import qualified Termlib.Variable as V
import Termlib.Problem.ParseErrors(ParseError(..), ParseWarning(..))
import Termlib.Term (Term(..))
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (liftM)
import Control.Monad.Error
import Control.Monad.Writer.Lazy
import Text.Parsec hiding (ParseError)

termFromString :: Signature -> String -> Either ParseError (Term,[ParseWarning])
termFromString sig input = case runWriter $ runErrorT $ runParserT term (sig,Map.empty,0) input input of 
                             (Left e,             _    ) -> Left e
                             (Right (Left e),     _    ) -> Left $ ParsecParseError e
                             (Right (Right t), warns) -> Right (t, warns)

type TermParser a = ParsecT String (Signature,Map String Variable, Int) (ErrorT ParseError (Writer [ParseWarning])) a

term :: TermParser Term
term = do s <- sym
          case s of 
            Left v  -> return $ Var v
            Right f -> Fun f `liftM` args
    where args = try (parens $ sepBy (whitespaced term) colon) <|> return []
getVar :: String -> TermParser Variable
getVar name = do (sig,vs,i) <- getState
                 case Map.lookup name vs of 
                   Just v -> return v
                   Nothing -> putState (sig,Map.insert name (Canon i) vs, i+1) >> return (Canon i)

sym :: TermParser (Either Variable Symbol)
sym = do name <- ident
         (sig,_,_) <- getState
         case F.symbol name sig of 
           Just s  -> return $ Right s
           Nothing -> Left `liftM` getVar name

colon = char ','
ident = many1 $ noneOf " \n\r\t()\",|-= "

whitespace = space <|> newline <|> tab <|> char '\r'

whitespaced p = do whitespace
                   f <- p
                   whitespace
                   return f

parens p = do char '('
              e <- p
              char ')'
              return e