module Termlib.Signature where
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad 
import Termlib.Utils

newtype Signature sym attribs = Signature (IntMap.IntMap attribs, Int) deriving Show

-- instance (Show sym, Show attribs) => Show (Signature sym attribs) where
--   show (Signature (sig,cnt,_)) = "Signature (" ++ show sig ++ "," ++ show cnt ++)"

empty :: Signature sym attribs
empty = Signature (IntMap.empty, 0)

elemAttrib :: Eq attribs => attribs -> Signature sym attribs -> Bool
elemAttrib attribs (Signature (sig, _)) = elem attribs . IntMap.elems $ sig

fresh :: Enumerateable sym => attribs -> Signature sym attribs -> (sym, Signature sym attribs)
fresh attribs (Signature (sig, counter)) = (invEnum counter', Signature (sig', counter'))
  where counter' = counter + 1
        sig' = IntMap.insert counter' attribs sig

findByAttribute :: (Enumerateable sym, MonadPlus m) => (attribs -> Bool) -> Signature sym attribs -> m sym
findByAttribute p (Signature (m,_)) = IntMap.foldWithKey f mzero m
  where f sym attrib r =  (if p attrib then return (invEnum sym) else mzero)  `mplus` r

fromAttrib :: (Enumerateable sym, Eq attribs) => attribs -> Signature sym attribs -> (sym, Signature sym attribs)
fromAttrib attribs s = case findByAttribute ((==) attribs) s of
                         Just sym -> (sym, s)
                         Nothing  -> fresh attribs s

attribute :: Enumerateable sym => (attribs -> a) -> sym -> Signature sym attribs -> a
attribute f s (Signature (m,_)) = f p where Just p = IntMap.lookup (enum s) m

attributes :: Enumerateable sym => sym -> Signature sym attribs -> attribs
attributes s (Signature (m,_)) = p where Just p = IntMap.lookup (enum s) m


symbols :: (Ord sym, Enumerateable sym) => Signature sym attribs -> Set sym
symbols (Signature (m, _)) = IntMap.foldWithKey f Set.empty m
  where f k _ = Set.insert (invEnum k)