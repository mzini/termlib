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

{-# LANGUAGE FlexibleInstances #-}

module Termlib.Problem.Parser ( 
  problemFromFile
  , problemFromString 
  ) where

import Control.Monad.Error
import System.IO.Error (catchIOError)
import System.FilePath (takeExtension)
import Termlib.Problem
import Termlib.Rule as R
import Termlib.Problem.ParseErrors
import qualified Termlib.Problem.TpdbParser as TpdbParser
import qualified Termlib.Problem.XmlParser as XmlParser


problemFromString :: String -> Maybe Problem
problemFromString input = case (XmlParser.problemFromString input, TpdbParser.problemFromString input) of 
                            (Right e, _)       -> Just $ fst' e
                            (_      , Right e) -> Just $ fst' e
                            _                  -> Nothing
  where fst' (a,_,_) = a
        
problemFromFile :: String -> IO (Either ParseError (Problem,[(Rule,Bool)],[ParseWarning]))
problemFromFile input = do minput <- getInputStr 
                           case minput of 
                             Just str -> return $ parser str
                             Nothing  -> return $ throwError $ ProblemNotFoundError input
    where getInputStr = catchIOError (Just `liftM` readFile input) (const $ return Nothing)
          parser | takeExtension input == ".trs" = TpdbParser.problemFromString
                 | takeExtension input == ".xml" = XmlParser.problemFromString
                 | otherwise                     = const $ throwError $ UnknownFileError input

