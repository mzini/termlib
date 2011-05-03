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

module Termlib.Repl where
import qualified Termlib.Problem.Parser as PParser
import qualified Termlib.Term.Parser as TParser
import Termlib.Problem (Problem, strictTRS, variables, signature)
import Termlib.Trs (Trs)
import Termlib.Term (Term)
import Control.Monad (liftM)

loadProblem :: String -> IO Problem
loadProblem input = do parsed <- PParser.problemFromFile input
                       case parsed of 
                         Right (prob,_) -> return $ prob
                         _              -> error "Repl.loadProblem"
                         
loadTrs :: String -> IO Trs
loadTrs input = do p <- loadProblem input
                   return $ strictTRS p

termFromString :: Problem -> String -> Term
termFromString prob str = case TParser.termFromString (signature prob) str of 
                            Right (t,_) -> t
                            Left e      -> error "Repl.termFromString"