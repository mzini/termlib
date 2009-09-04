module Termlib.Trs
where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf)
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

singleton :: Rule -> Trs
singleton r = Trs [r]

invert :: Trs -> Trs
invert = Trs . map R.invert . rules

lhss :: Trs -> [T.Term]
lhss = map R.lhs . rules

rhss :: Trs -> [T.Term]
rhss = map R.rhs . rules

union :: Trs -> Trs -> Trs
(Trs trs1) `union` (Trs trs2) = Trs $ trs1 ++ trs2

(\\) :: Trs -> Trs -> Trs
(Trs trs1) \\ (Trs trs2) = Trs $ trs1 List.\\ trs2

wellFormed :: Trs -> Bool
wellFormed = allrules wf
    where wf r = not (T.isVariable lhs) && (T.variables rhs `Set.isSubsetOf` T.variables lhs)
              where lhs = R.lhs r
                    rhs = R.rhs r

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

mapRules :: (Rule -> Rule) -> Trs -> Trs
mapRules f = Trs . map f . rules

filterRules :: (Rule -> Bool) -> Trs -> Trs
filterRules f = Trs . filter f . rules

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
constructors trs =  functionSymbols trs Set.\\ definedSymbols trs


-- rewrites s t trs = any (R.rewrites s t) $ rules trs

-- topRewrites s t trs = any (R.topRewrites s t) $ rules trs

-- reduced = allrules . R.reduced

-- topReduced = allrules . R.topReduced

isDuplicating :: Trs -> Bool
isDuplicating trs = any R.isDuplicating $ rules trs

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

isRightLinear :: Trs -> Bool
isRightLinear = allrules R.isRightLinear

-- leftGround = allrules R.leftGround

-- rightGround = allrules R.rightGround

isConstructor :: Trs -> Bool
isConstructor trs = allrules (cb . R.lhs) trs
    where cb (T.Fun f ts) = all (\ ti -> T.functionSymbols ti `isSubsetOf` constrs) ts
          cb _          = False
          constrs = constructors trs

isOverlapping :: Trs -> Bool
isOverlapping (Trs rs) = R.isAnyOverlapping rs rs
