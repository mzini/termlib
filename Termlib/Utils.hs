module Termlib.Utils where 

import Text.PrettyPrint.HughesPJ

class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a


class PrettyPrintable a where
  pprint :: a -> Doc 