module Rule
  (
  Rule
  ) where

import qualified Term as T
import qualified Data.Map as Map

data Rule = Rule {lhs :: T.Term, rhs :: T.Term} deriving Show

equal r1 r2 = r1 == r2

instance Eq Rule where
  r1 == r2 = let (lhs1, vm1) = ((`T.canonise` Map.empty) . lhs) r1
                 (lhs2, vm2) = ((`T.canonise` Map.empty) . lhs) r2
                 canonrhs vm = fst . (`T.canonise` vm) . rhs in
             lhs1 == lhs2 && canonrhs vm1 r1 == canonrhs vm2 r2