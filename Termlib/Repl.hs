module Termlib.Repl where
import qualified Termlib.Problem.Parser as PParser
import Termlib.Problem (Problem, strictTrs)
import Termlib.Trs (Trs)
import Control.Monad (liftM)
loadProblem :: String -> IO (Maybe Problem) 
loadProblem input = do parsed <- PParser.problemFromFile input
                       case parsed of 
                         Right (prob,_) -> return $ Just $ prob
                         _              -> return $ Nothing                 
                         
loadTrs :: String -> IO (Maybe Trs) 
loadTrs input = do p <- loadProblem input
                   return $ strictTrs `liftM` p