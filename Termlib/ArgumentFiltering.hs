module Termlib.ArgumentFiltering 
where
import qualified Data.IntSet as Set
import Data.IntSet (IntSet, fromList)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ hiding (empty)
import qualified Text.PrettyPrint.HughesPJ as PP
import Termlib.FunctionSymbol (Signature, Symbol, emptySignature, arity, maybeFresh, defaultAttribs, symbolName)
import Termlib.Utils (PrettyPrintable(..))
import Termlib.Trs (fromRules, Trs, rules)
import Termlib.Signature (runSignature,  getSignature)
import Control.Monad (liftM, liftM2)
import Termlib.Rule (Rule(..))
import Termlib.Term (Term (..))

data Filtering = Projection Int
               | Filtering IntSet deriving (Eq, Show)

newtype ArgumentFiltering = AF (Signature, Map Symbol Filtering) 
  deriving (Eq, Show)

empty :: Signature -> ArgumentFiltering
empty sig = AF (sig, Map.empty)

filtering :: Symbol -> ArgumentFiltering -> Filtering
filtering f (AF (sig,m)) = case Map.lookup f m of 
                             Just e  -> e
                             Nothing -> Filtering $ fromList (take ar [1..])
                                 where ar = arity sig f

instance PrettyPrintable ArgumentFiltering where 
  pprint (AF (sig, m)) | m == Map.empty = text "empty"
                       | otherwise      = fsep $ punctuate (text ",")  [ppp s f | (s,f) <- Map.toList m]
    where ppp s f = text "pi" <> parens (pprint (s, sig)) <+> text "=" <+> ppf f 
          ppf (Projection i) = text $ show i
          ppf (Filtering is) = brackets $ sep $ punctuate (text ",") $ [ text $ show i | i <- Set.toList $ is]

alter :: (Maybe Filtering -> Maybe Filtering) -> Symbol -> ArgumentFiltering -> ArgumentFiltering
alter f s (AF (sig, m)) = AF (sig, Map.alter f s m)

apply :: Trs -> ArgumentFiltering -> (Trs, Signature)
apply trs af = runSignature (fromRules `liftM` mapM filterRule (rules trs)) emptySignature
    where filterRule (Rule lhs rhs) = liftM2 Rule (filter lhs) (filter rhs)
          filter (Var x)    = return $ Var x
          filter (Fun f ts) = case filtering f af of 
                                Projection i -> filter $ ts!!(i-1)
                                Filtering is -> do {f' <- mkFreshSym;
                                                   ts'' <- mapM filter ts';
                                                   return $ Fun f' ts''}
                                    where ts'        = snd $ foldl (\ (i,ts') ti -> if Set.member i is then (i+1,ti:ts') else (i+1,ts))  (1,[]) ts
                                          mkFreshSym = do sig <- getSignature 
                                                          maybeFresh $ defaultAttribs (symbolName sig f) (length ts')
