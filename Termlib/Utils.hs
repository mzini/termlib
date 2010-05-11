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
module Termlib.Utils where 

import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types

class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a


class PrettyPrintable a where
    pprint :: a -> Doc

instance PrettyPrintable Doc where
    pprint = id

-- class CpfPrintable a where
--    cpfprint :: a -> Element

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing


eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM ma mb me = do e <- me 
                      either ma mb  e
