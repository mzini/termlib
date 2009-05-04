module Termlib.Utils where 

import Text.PrettyPrint.HughesPJ

class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a


class PrettyPrintable a where
    pprint :: a -> Doc 

instance PrettyPrintable Doc where
    pprint = id

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing

