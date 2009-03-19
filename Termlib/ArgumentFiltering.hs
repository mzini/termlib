module Termlib.ArgumentFiltering 
where
import qualified Data.IntSet as Set
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ hiding (empty)
import qualified Text.PrettyPrint.HughesPJ as PP
import Termlib.FunctionSymbol (Signature, Symbol)
import Termlib.Utils (PrettyPrintable(..))

data Filtering = Projection Int
               | Filtering IntSet deriving (Eq, Show)

newtype ArgumentFiltering = AF (Signature, Map Symbol Filtering) 
  deriving (Eq, Show)

empty :: Signature -> ArgumentFiltering
empty sig = AF (sig, Map.empty)

instance PrettyPrintable ArgumentFiltering where 
  pprint (AF (sig, m)) = Map.foldWithKey (\ s f d -> ppe s f $$ d) PP.empty m 
    where ppe s f = fsep [ text "pi" <> parens (pprint (s, sig))
                         , text "="
                         , ppf f]
          ppf (Projection i) = text $ show i
          ppf (Filtering is) = braces $ sep $ punctuate (text ",") $ [ text $ show i | i <- Set.toList $ is]

alter :: (Maybe Filtering -> Maybe Filtering) -> Symbol -> ArgumentFiltering -> ArgumentFiltering
alter f s (AF (sig, m)) = AF (sig, Map.alter f s m)