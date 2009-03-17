module Termlib.Trs
  -- (
  -- Trs,
  -- isEmpty, 
  -- empty,
  -- makeTrs, 
  -- allrules,
  -- definedSymbols,
  -- constructors,
  -- rewrites,
  -- topRewrites,
  -- reduced,
  -- topReduced,
  -- duplicating,
  -- nonduplicating,
  -- flat,
  -- shallow,
  -- linear,
  -- ground,
  -- leftFlat,
  -- leftShallow,
  -- leftLinear,
  -- leftGround,
  -- rightFlat,
  -- rightShallow,
  -- rightLinear,
  -- rightGround,
  -- ) 
where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Control.Monad.State.Lazy as State

import qualified Termlib.Rule as R
import Termlib.Rule (Rule)
import qualified Termlib.Term as T
import Termlib.Term (Term)
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Symbol, Signature)

import qualified Termlib.Variable as V
import Termlib.Variable (Variables, Variable)
import qualified Termlib.Signature as Signature

type Rules = [R.Rule]
data Trs = Trs {rules :: Rules}
           deriving (Eq, Show)

empty :: Trs
empty = Trs [] 

fromRules :: Rules -> Trs
fromRules = Trs

isEmpty :: Trs -> Bool
isEmpty trs = rules trs == []

allrules :: (Rule -> Bool) -> Trs -> Bool
allrules f trs = all f $ rules trs

foldlRules :: (a -> Rule -> a) -> a -> Trs -> a 
foldlRules f a = foldl f a . rules

foldrRules :: (Rule -> a -> a) -> a -> Trs -> a 
foldrRules f a = foldr f a . rules

insert :: R.Rule -> Trs -> Trs
insert r (Trs rs) = Trs $ r : List.delete r rs

variables :: Trs -> Set Variable
variables = foldlRules (\ s r -> s `Set.union` (R.variables r)) Set.empty

functionSymbols :: Trs -> Set Symbol 
functionSymbols = foldlRules (\ s r -> s `Set.union` (R.functionSymbols r)) Set.empty

definedSymbols :: Trs -> Set Symbol
definedSymbols trs = foldlRules f Set.empty trs
  where f s (R.Rule l _) = case T.root l of 
                             Left _  -> error "Trs.definedSymbols. Variable as lhs"
                             Right r -> Set.insert r s

constructors :: Trs -> Set F.Symbol
constructors trs =  functionSymbols trs \\ definedSymbols trs


-- rewrites s t trs = any (R.rewrites s t) $ rules trs

-- topRewrites s t trs = any (R.topRewrites s t) $ rules trs

-- reduced = allrules . R.reduced

-- topReduced = allrules . R.topReduced

-- duplicating trs = any R.duplicating $ rules trs

-- nonduplicating = allrules R.nonduplicating

-- flat = allrules R.flat

-- shallow = allrules R.shallow

-- linear = allrules R.linear

-- ground = allrules R.ground

-- leftFlat = allrules R.leftFlat

-- rightFlat = allrules R.rightFlat

-- leftShallow = allrules R.leftShallow

-- rightShallow = allrules R.rightShallow

-- leftLinear = allrules R.leftLinear

-- rightLinear = allrules R.rightLinear

-- leftGround = allrules R.leftGround

-- rightGround = allrules R.rightGround
