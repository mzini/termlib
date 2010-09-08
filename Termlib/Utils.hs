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

{-# LANGUAGE FlexibleContexts #-}
module Termlib.Utils where 

-- import Control.Monad.Identity ()
import Text.PrettyPrint.HughesPJ
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String ()


-- * Parsing and Printing

class PrettyPrintable a where
  pprint :: a -> Doc

instance PrettyPrintable Doc where
  pprint = id

class Parsable a where
  parse :: Stream s m Char => ParsecT s u m a

parseFromString :: Parsable a => String -> Either ParseError a
parseFromString s = runParser parse () "/dev/stderr" s


-- * Memoisation

type MemoAction k a = State.State (Map.Map k a)

memo :: (Ord k) => k -> (MemoAction k a a) -> (MemoAction k a a)
memo k  m = do s <- State.get 
               case Map.lookup k s of
                 Just old -> return old
                 Nothing  -> do { new <- m;
                                 State.modify (Map.insert k new);
                                 return new}


runMemoAction :: (Ord k) => MemoAction k a b -> b
runMemoAction ma = fst $ State.runState ma Map.empty

liftMemo :: (Ord k) => (k -> a) -> (k -> MemoAction k a a)
liftMemo f k = memo k (return $ f k)

-- * List Utility Functions

listProduct :: [[a]] -> [[a]]
listProduct []             = [[]]
listProduct (xs:xss) = foldl f [] xs
  where f a x = map (\ xs' -> x:xs') (listProduct xss) ++ a


snub :: Ord a => [a] -> [a]
snub = Set.toList . Set.fromList


-- * Monad Utilities

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing


eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM ma mb me = do e <- me 
                      either ma mb  e

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = do g <- b
               if g then t else e

-- * Parsec Utility Functions


($++$) :: Doc -> Doc -> Doc
a $++$ b = a $+$ text "" $+$ b

paragraph :: String -> Doc
paragraph s = fsep [text w | w <- words s]

underline :: Doc -> Doc
underline p = p $+$ text (take (length $ show p) $ repeat '-')

-- * Misc
class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a
