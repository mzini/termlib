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
