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

module Termlib.ArgumentFiltering 
where
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.IntSet (IntSet, fromList, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ hiding (empty)
import qualified Text.PrettyPrint.HughesPJ as PP
import Termlib.FunctionSymbol (Signature, Symbol, symbols, emptySignature, arity, maybeFresh, defaultAttribs, symbolName, SignatureMonad, getAttributes, Attributes (..))
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

fold :: (Symbol -> Filtering -> b -> b) -> ArgumentFiltering -> b -> b
fold f af@(AF (sig, _)) m = foldr f' m (Set.toList $ symbols sig)
  where f' sym = f sym (filtering sym af)

instance PrettyPrintable ArgumentFiltering where 
  pprint (AF (sig, m)) | m == Map.empty = text "empty"
                       | otherwise      = fsep $ punctuate (text ",")  [ppp s f | (s,f) <- Map.toList m]
    where ppp s f = text "pi" <> parens (pprint (s, sig)) <+> text "=" <+> ppf f 
          ppf (Projection i) = text $ show i
          ppf (Filtering is) = brackets $ sep $ punctuate (text ",") $ [ text $ show i | i <- IntSet.toList $ is]

alter :: (Maybe Filtering -> Maybe Filtering) -> Symbol -> ArgumentFiltering -> ArgumentFiltering
alter f s (AF (sig, m)) = AF (sig, Map.alter f s m)

apply :: Trs -> ArgumentFiltering -> SignatureMonad Trs
apply trs af = fromRules `liftM` mapM filterRule (rules trs)
    where filterRule (Rule lhs rhs) = liftM2 Rule (filter lhs) (filter rhs)
          filter (Var x)    = return $ Var x
          filter (Fun f ts) = case filtering f af of 
                                Projection i -> filter $ ts!!(i-1)
                                Filtering is -> do {f' <- mkFreshSym;
                                                   ts'' <- mapM filter ts';
                                                   return $ Fun f' ts''}
                                    where ts'        = snd $ foldl (\ (i,ts') ti -> if IntSet.member i is then (i+1,ti:ts') else (i+1,ts))  (1,[]) ts
                                          mkFreshSym = do attribs <- getAttributes f
                                                          maybeFresh $ attribs { symArity = IntSet.size is}
