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

{-# LANGUAGE TypeSynonymInstances #-}

module Termlib.Trs
where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Set (Set, isSubsetOf)
import Control.Monad.State.Lazy as S
import qualified Termlib.Rule as R
import qualified Termlib.Variable as V
import Termlib.Rule (Rule)
import qualified Termlib.Term as T
import Termlib.Term (Term)
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Symbol)
import qualified Data.Foldable as Fold
import Termlib.Variable (Variable)

data RuleSet a = Trs {ruleSet :: Set a}
               deriving (Eq, Show)



instance Fold.Foldable RuleSet where
  foldr f b (Trs rs) = Set.foldr f b rs

type Trs = RuleSet Rule

rules :: Trs -> [Rule]
rules = Set.elems . ruleSet

empty :: Trs
empty = Trs Set.empty

singleton :: Rule -> Trs
singleton = Trs . Set.singleton

invert :: Trs -> Trs
invert = fromRules . map R.invert . rules

lhss :: Trs -> [T.Term]
lhss = map R.lhs . rules

rhss :: Trs -> [T.Term]
rhss = map R.rhs . rules

append :: Trs -> Trs -> Trs
append = union

union :: Trs -> Trs -> Trs
(Trs trs1) `union` (Trs trs2) = Trs $ trs1 `Set.union` trs2

unions :: [Trs] -> Trs
unions = List.foldl union empty

intersect :: Trs -> Trs -> Trs
Trs rs1 `intersect` Trs rs2 = Trs $ rs1 `Set.intersection` rs2
    
member :: Trs -> Rule -> Bool 
member (Trs trs) r = r `Set.member` trs

(\\) :: Trs -> Trs -> Trs
(Trs trs1) \\ (Trs trs2) = Trs $ trs1 Set.\\ trs2

wellFormed :: Trs -> Bool
wellFormed = Fold.all wf
    where wf r = not (T.isVariable lhs) && (T.variables rhs `Set.isSubsetOf` T.variables lhs)
              where lhs = R.lhs r
                    rhs = R.rhs r

fromRules :: [Rule] -> Trs
fromRules = Trs . Set.fromList

toRules :: Trs -> [Rule]
toRules (Trs rs) = Set.elems rs

isEmpty :: Trs -> Bool
isEmpty trs = rules trs == []

mapRules :: (Rule -> Rule) -> Trs -> Trs
mapRules f = fromRules . map f . rules

mapTerms :: (Term -> Term) -> Trs -> Trs
mapTerms f rs = fromRules [R.Rule (f lh) (f rh) | R.Rule lh rh <- rules rs ]

filterRules :: (Rule -> Bool) -> Trs -> Trs
filterRules f (Trs rs) = Trs $ Set.filter f rs

insert :: R.Rule -> Trs -> Trs
insert r (Trs rs) = Trs $ Set.insert r rs

variables :: Trs -> Set Variable
variables = Fold.foldl (\ s r -> s `Set.union` (R.variables r)) Set.empty

functionSymbols :: Trs -> Set Symbol 
functionSymbols = Fold.foldl (\ s r -> s `Set.union` (R.functionSymbols r)) Set.empty

definedSymbols :: Trs -> Set Symbol
definedSymbols trs = Fold.foldl f Set.empty trs
  where f s (R.Rule l _) = case T.root l of 
                             Left _  -> error "Trs.definedSymbols. Variable as lhs"
                             Right r -> Set.insert r s

constructors :: Trs -> Set F.Symbol
constructors trs =  functionSymbols trs Set.\\ definedSymbols trs

definingSymbol :: Trs -> F.Symbol -> Trs
definingSymbol trs f = filterRules (\ rl -> T.root (R.lhs rl) == Right f) trs

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
isGround = Fold.all R.isGround

-- leftFlat = Fold.all R.leftFlat

-- rightFlat = Fold.all R.rightFlat

-- leftShallow = Fold.all R.leftShallow

-- rightShallow = Fold.all R.rightShallow

isLeftLinear :: Trs -> Bool
isLeftLinear = Fold.all R.isLeftLinear

isRightLinear :: Trs -> Bool
isRightLinear = Fold.all R.isRightLinear

-- leftGround = Fold.all R.leftGround

-- rightGround = Fold.all R.rightGround

isSizeIncreasing :: Trs -> Bool
isSizeIncreasing trs = any R.isSizeIncreasing $ rules trs

isNonSizeIncreasing :: Trs -> Bool
isNonSizeIncreasing = Fold.all R.isNonSizeIncreasing

isConstructor :: Trs -> Bool
isConstructor trs = Fold.all (cb . R.lhs) trs
    where cb (T.Fun _ ts) = all (\ ti -> T.functionSymbols ti `isSubsetOf` constrs) ts
          cb _          = False
          constrs = constructors trs

isCollapsing :: Trs -> Bool
isCollapsing trs = any R.isCollapsing $ rules trs

isNestedRecursive :: Trs -> Bool
isNestedRecursive trs = any nr rs
    where nr (R.Rule l r) = any hasNestedRoot [t | t <- T.subterms r, T.root t == T.root l]
          rs = rules trs
          hasNestedRoot (T.Var _)    = False
          hasNestedRoot (T.Fun f ts) = f `Set.member` Set.unions [T.functionSymbols ti | ti <- ts]


overlaps :: Trs -> [R.Overlap]
overlaps trs = 
  concatMap (uncurry R.overlaps) [ (r1,r2) 
                                 | (i1,r1) <- rs'
                                 , (i2,r2) <- rs'
                                 , i1 <= i2]
  where rs' = zip [(1::Int)..] $ rules trs
        
isOverlapping :: Trs -> Bool
isOverlapping = not . null . overlaps

isOverlay :: Trs -> Bool
isOverlay = all rootOverlap . overlaps
  where rootOverlap (_,[],_) = True
        rootOverlap _        = False
        
isOrthogonal :: Trs -> Bool        
isOrthogonal trs = isLeftLinear trs && not (isOverlapping trs)

-- type Position = [Int]
-- type Overlap = (Rule, Position, Rule)

-- rename :: Term -> State Int Term
-- rename t = fst `liftM` rn t Map.empty
--   where rn (T.Var x) m = 
--           case Map.lookup x m of 
--             Just x' -> return (T.Var x', m)
--             Nothing -> 
--               do x' <- fresh
--                  return (T.Var x', Map.insert x x' m)
--         rn (T.Fun f ts) m = 
--           do (ts', m') <- foldM rnl ([],m) ts
--              return (T.Fun f ts', m)
--                where rnl (ts',m') ti =
--                        do (ti',m'') <- rn ti m'
--                           return (ti':ts',m'')
--         fresh = 
--           do fr <- get
--              put $ fr + 1
--              return $ V.Canon fr

-- overlaps :: Trs -> [Overlap]
-- overlaps (Trs rs) = concatMap (uncurry ovs) [(r1', r2') | r1' <- rs', r2' <- rs']
--   where ovs (l1,r1) (l2,r2) = undefined
--         rs' = evalState (mapM rn rs) 0
--         rn r = 
--           do l <- rename (R.lhs r)
--              return (l,r)