module Termlib.Utils where 

class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a
 