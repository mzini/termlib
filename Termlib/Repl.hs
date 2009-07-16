module Termlib.Repl where
import qualified Termlib.Problem.Parser as PParser
import qualified Termlib.Term.Parser as TParser
import Termlib.Problem (Problem, strictTrs, variables, signature)
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
                   return $ strictTrs p

termFromString :: Problem -> String -> Term
termFromString prob str = case TParser.termFromString (signature prob) str of 
                            Right (t,_) -> t
                            Left e      -> error "Repl.termFromString"