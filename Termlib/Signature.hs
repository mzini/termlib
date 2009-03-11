module Termlib.Signature where
import qualified Data.IntMap as IntMap
import Control.Monad 
import Termlib.Utils

newtype Signature sym prop = Signature (IntMap.IntMap prop, Int) deriving Show

-- instance (Show sym, Show prop) => Show (Signature sym prop) where
--   show (Signature (sig,cnt,_)) = "Signature (" ++ show sig ++ "," ++ show cnt ++)"

empty :: Signature sym prop
empty = Signature (IntMap.empty, 0)

fresh :: Enumerateable sym => prop -> Signature sym prop -> (sym, Signature sym prop)
fresh attribs (Signature (sig, counter)) = (invEnum counter', Signature (sig', counter'))
  where counter' = counter + 1
        sig' = IntMap.insert counter' attribs sig

findByAttribute :: (Enumerateable sym, MonadPlus m) => (prop -> Bool) -> Signature sym prop -> m sym
findByAttribute p (Signature (m,_)) = IntMap.foldWithKey f mzero m
  where f sym attrib r =  (if p attrib then return (invEnum sym) else mzero)  `mplus` r

fromAttrib :: (Enumerateable sym, Eq prop) => prop -> Signature sym prop -> (sym, Signature sym prop)
fromAttrib attribs s = case findByAttribute ((==) attribs) s of
                         Just sym -> (sym, s)
                         Nothing  -> fresh attribs s

attribute :: Enumerateable sym => (prop -> a) -> sym -> Signature sym prop -> a
attribute f s (Signature (m,_)) = f p where Just p = IntMap.lookup (enum s) m

attributes :: Enumerateable sym => sym -> Signature sym prop -> prop
attributes s (Signature (m,_)) = p where Just p = IntMap.lookup (enum s) m
