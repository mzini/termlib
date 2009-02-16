module Termlib.Rule
  (
  bothsides,
  lhsVariables,
  rhsVariables,
  isRewriteRule,
  duplicating,
  nonduplicating,
  flat,
  shallow,
  linear,
  ground,
  leftFlat,
  leftShallow,
  leftLinear,
  leftGround,
  rightFlat,
  rightShallow,
  rightLinear,
  rightGround,
  Rule
  ) where

import qualified Termlib.Term as T
import qualified Data.List as List
import qualified Data.Map as Map

data Rule = Rule {lhs :: T.Term, rhs :: T.Term} deriving Show

instance Eq Rule where
  r1 == r2 = lhs1 == lhs2 && canonrhs vm1 r1 == canonrhs vm2 r2
    where (lhs1, vm1) = ((`T.canonise` Map.empty) . lhs) r1
          (lhs2, vm2) = ((`T.canonise` Map.empty) . lhs) r2
          canonrhs vm = fst . (`T.canonise` vm) . rhs

bothsides f r = (f . lhs) r && (f . rhs) r

lhsVariables = T.variables . lhs

rhsVariables = T.variables . rhs

isRewriteRule r = (not . T.isVariable . lhs) r && all (`elem` lvar) rvar
  where lvar = lhsVariables r
        rvar = rhsVariables r

duplicating r = any (\x -> T.varCardinality x (lhs r) < T.varCardinality x (rhs r)) lvar
  where lvar = lhsVariables r

nonduplicating r = all (\x -> T.varCardinality x (lhs r) >= T.varCardinality x (rhs r)) lvar
  where lvar = lhsVariables r

flat = bothsides T.flat

shallow = bothsides T.shallow

linear = bothsides T.linear

ground = bothsides T.ground

leftFlat = T.flat . lhs

rightFlat = T.flat . rhs

leftShallow = T.shallow . lhs

rightShallow = T.shallow . rhs

leftLinear = T.linear . lhs

rightLinear = T.linear . rhs

leftGround = T.ground . lhs

rightGround = T.ground . rhs