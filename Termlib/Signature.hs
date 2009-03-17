{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Termlib.Signature 
  ( Signature
  , SignatureMonad
  , empty
  , cardinality 
  , fresh
  , maybeFresh
  , runSignature
  , findByAttribute
  , attribute
  , attributes
  , symbols
  , liftS
  )
where
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad
import Control.Monad.Trans
import Termlib.Utils
import qualified Control.Monad.State.Lazy as State

newtype Signature sym attribs = Signature (IntMap.IntMap attribs, Int) deriving Show

newtype SignatureMonad sym attribs a = SignatureMonad {
    runSig :: State.State (Signature sym attribs) a
  } deriving (Monad)

getSignature :: SignatureMonad sym attrib (Signature sym attrib)
getSignature = SignatureMonad State.get

putSignature :: Signature sym attrib -> SignatureMonad sym attrib ()
putSignature = SignatureMonad . State.put


liftS :: (Signature sym attribs -> a) -> SignatureMonad sym attribs a
liftS f = f `liftM` getSignature

modifySignature :: (Signature sym attribs -> Signature sym attribs) -> SignatureMonad sym attribs (Signature sym attribs)
modifySignature f = do sig <- getSignature
                       let sig' = f sig
                       putSignature sig'
                       return sig'

runSignature :: SignatureMonad sym attribs a -> Signature sym attribs -> (a, Signature sym attribs)
runSignature = State.runState . runSig

empty :: Signature sym attribs
empty = Signature (IntMap.empty, 0)

cardinality :: Signature sym attribs -> Int 
cardinality (Signature (_, c)) = c


fresh :: Enumerateable sym => attribs -> SignatureMonad sym attribs sym
fresh attribs = do (invEnum . cardinality) `liftM` modifySignature f
  where f (Signature (sig, counter)) = Signature (sig', counter')
          where counter' = counter + 1
                sig' = IntMap.insert counter' attribs sig

maybeFresh :: (Enumerateable sym, Eq attribs) => attribs -> SignatureMonad sym attribs sym
maybeFresh attribs = do sig <- getSignature 
                        case findByAttribute ((==) attribs) sig of
                          Just sym -> return sym
                          Nothing  -> fresh attribs

findByAttribute :: (Enumerateable sym, MonadPlus m) => (attribs -> Bool) -> Signature sym attribs -> m sym
findByAttribute p (Signature (m,_)) = IntMap.foldWithKey f mzero m
  where f sym attrib r =  (if p attrib then return (invEnum sym) else mzero)  `mplus` r


attribute :: Enumerateable sym => (attribs -> a) -> sym -> Signature sym attribs -> a
attribute f s (Signature (m,_)) = f p where Just p = IntMap.lookup (enum s) m

attributes :: Enumerateable sym => sym -> Signature sym attribs -> attribs
attributes = attribute id


symbols :: (Ord sym, Enumerateable sym) => Signature sym attribs -> Set sym
symbols (Signature (m, _)) = IntMap.foldWithKey f Set.empty m
  where f k _ = Set.insert (invEnum k)