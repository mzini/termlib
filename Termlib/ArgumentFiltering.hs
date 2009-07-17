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
  pprint (AF (sig, m)) | m == Map.empty = text "empty"
                       | otherwise      = hsep $ punctuate (text ",")  [ppp s f | (s,f) <- Map.toList m]
    where ppp s f = text "pi" <> parens (pprint (s, sig)) <+> text "=" <+> ppf f 
          ppf (Projection i) = text $ show i
          ppf (Filtering is) = brackets $ sep $ punctuate (text ",") $ [ text $ show i | i <- Set.toList $ is]

alter :: (Maybe Filtering -> Maybe Filtering) -> Symbol -> ArgumentFiltering -> ArgumentFiltering
alter f s (AF (sig, m)) = AF (sig, Map.alter f s m)

