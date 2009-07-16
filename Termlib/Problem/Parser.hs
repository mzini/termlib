module Termlib.Problem.Parser ( problemFromFile ) where
import Control.Monad.Error

import Control.Monad.Error
import System.FilePath (takeExtension)
import Termlib.Problem
import Termlib.Problem.ParseErrors
import qualified Termlib.Problem.TpdbParser as TpdbParser
import qualified Termlib.Problem.XmlParser as XmlParser


problemFromString :: String -> Maybe Problem
problemFromString input = case (XmlParser.problemFromString input, TpdbParser.problemFromString input) of 
                            (Right e,_)        -> Just $ fst e
                            (_      , Right e) -> Just $ fst e
                            _                  -> Nothing

problemFromFile :: String -> IO (Either ParseError (Problem,[ParseWarning]))
problemFromFile input = do minput <- getInputStr 
                           case minput of 
                             Just str -> return $ parser str
                             Nothing  -> return $ throwError $ ProblemNotFoundError input
    where getInputStr = catch (Just `liftM` readFile input) (const $ return Nothing)
          parser | takeExtension input == ".trs" = TpdbParser.problemFromString
                 | takeExtension input == ".xml" = XmlParser.problemFromString
                 | otherwise                     = const $ throwError $ UnknownFileError input

