module TRS
  (
  TRS
  ) where

import qualified Rule as R
import qualified Data.Set as Set

newtype TRS = TRS (Set.Set R.Rule)