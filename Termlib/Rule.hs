module Rule
  (
  Rule
  ) where

import qualified Term as T

data Rule = Rule {lhs :: T.Term, rhs :: T.Term}

equal r1 r2 = r1 == r2

instance Eq Rule where
  (==) = equal

instance Ord Rule where
  (<=) = equal