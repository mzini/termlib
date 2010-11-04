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

module Termlib.Trs
where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf)

import qualified Termlib.Rule as R
import Termlib.Rule (Rule)
import qualified Termlib.Term as T
import Termlib.Term (Term)
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Symbol)

import Termlib.Variable (Variable)

type Rules = [R.Rule]
data Trs = Trs {rules :: Rules}
           deriving (Eq, Show)

liftTrs :: (Rules -> a) -> Trs -> a
liftTrs f (Trs trs) = f trs

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

append :: Trs -> Trs -> Trs
(Trs trs1) `append` (Trs trs2) = Trs $ trs1 ++ trs2

union :: Trs -> Trs -> Trs
(Trs trs1) `union` (Trs trs2) = Trs $ foldl ins trs2 trs1 
    where ins [] r = [r]
          ins (r:rs) r' | r == r'   = r:rs
                        | otherwise = r : ins rs r'
unions :: [Trs] -> Trs
unions = foldl union empty

(\\) :: Trs -> Trs -> Trs
(Trs trs1) \\ (Trs trs2) = Trs $ trs1 List.\\ trs2

wellFormed :: Trs -> Bool
wellFormed = allrules wf
    where wf r = not (T.isVariable lhs) && (T.variables rhs `Set.isSubsetOf` T.variables lhs)
              where lhs = R.lhs r
                    rhs = R.rhs r

fromRules :: Rules -> Trs
fromRules = Trs

toRules :: Trs -> Rules
toRules (Trs rs) = rs

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

mapTerms :: (Term -> Term) -> Trs -> Trs
mapTerms f (Trs rs) = Trs [R.Rule (f lh) (f rh) | R.Rule lh rh <- rs ]

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

isGround :: Trs -> Bool
isGround = allrules R.isGround

-- leftFlat = allrules R.leftFlat

-- rightFlat = allrules R.rightFlat

-- leftShallow = allrules R.leftShallow

-- rightShallow = allrules R.rightShallow

isLeftLinear :: Trs -> Bool
isLeftLinear = allrules R.isLeftLinear

isRightLinear :: Trs -> Bool
isRightLinear = allrules R.isRightLinear

-- leftGround = allrules R.leftGround

-- rightGround = allrules R.rightGround

isSizeIncreasing :: Trs -> Bool
isSizeIncreasing trs = any R.isSizeIncreasing $ rules trs

isNonSizeIncreasing :: Trs -> Bool
isNonSizeIncreasing = allrules R.isNonSizeIncreasing

isConstructor :: Trs -> Bool
isConstructor trs = allrules (cb . R.lhs) trs
    where cb (T.Fun _ ts) = all (\ ti -> T.functionSymbols ti `isSubsetOf` constrs) ts
          cb _          = False
          constrs = constructors trs

isOverlapping :: Trs -> Bool
isOverlapping (Trs rs) = R.isAnyOverlapping rs rs

isCollapsing :: Trs -> Bool
isCollapsing trs = any R.isCollapsing $ rules trs

isNestedRecursive :: Trs -> Bool
isNestedRecursive (Trs rs) = any nr rs
    where nr (R.Rule l r) = any hasNestedRoot [t | t <- T.subterms r, T.root t == T.root l]
          
          hasNestedRoot (T.Var _)    = False
          hasNestedRoot (T.Fun f ts) = f `Set.member` Set.unions [T.functionSymbols ti | ti <- ts]
