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

module Termlib.ContextSensitive 
    ( ReplacementMap
    , empty
    , replacingPositions
    , setReplacing )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Map (Map)
import qualified Data.Map as Map
import Termlib.FunctionSymbol (Signature, Symbol)


newtype ReplacementMap = RM (Signature, Map Symbol IntSet) deriving (Eq, Show)

empty :: Signature -> ReplacementMap
empty sig  = RM (sig, Map.empty)

replacingPositions :: Symbol -> ReplacementMap -> [Int]
replacingPositions f (RM (_, m)) = 
    case Map.lookup f m of 
      Just poss -> ISet.toList poss
      Nothing   -> []

setReplacing :: Symbol -> [Int] -> ReplacementMap -> ReplacementMap
setReplacing f poss (RM (sig, m)) = RM (sig, Map.alter alter f m)
  where alter (Just s) = Just $ foldl (flip ISet.insert) s poss
        alter Nothing = Just $ ISet.fromList poss