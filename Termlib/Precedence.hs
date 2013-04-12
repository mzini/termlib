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

{-# LANGUAGE DeriveDataTypeable #-}

module Termlib.Precedence where
import Termlib.FunctionSymbol 
import Text.PrettyPrint.HughesPJ
import Termlib.Utils (PrettyPrintable(..))
import Data.Typeable 
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as St
import Data.List (find)

data Order b = b :>: b
             | b :~: b deriving (Show, Eq, Ord, Typeable)

newtype Precedence = Precedence (Signature,[Order Symbol]) deriving Show

instance PrettyPrintable Precedence where 
  pprint (Precedence (_, [])) = text "empty"
  pprint (Precedence (sig,l)) = fsep $ punctuate (text ",") [pp e | e <- l] 
    where pp (f :>: g) = pprint (f,sig) <+> text ">" <+> pprint (g,sig)
          pp (f :~: g) =  pprint (f,sig) <+> text "~" <+> pprint (g,sig)

precedence :: Signature -> [Order Symbol] -> Precedence
precedence = curry Precedence 

empty :: Signature -> Precedence
empty sig = precedence sig []

insert :: Order Symbol -> Precedence -> Precedence
insert e (Precedence (sig, l)) = Precedence (sig, e : l)

eclasses :: Precedence -> [Set.Set Symbol]
eclasses (Precedence (_, l)) = foldl ins [] l
  where ins []          (g :~: h) = [Set.fromList [g,h]]
        ins (ec:ecs) eq@(g :~: h) | g `Set.member` ec = h `Set.insert` ec : ecs
                                  | h `Set.member` ec = g `Set.insert` ec : ecs
                                  | otherwise         = ec : ins ecs eq
        ins ecs _                 = ecs
                                                
recursionDepth :: Set.Set Symbol -> Precedence -> Map.Map Symbol Int
recursionDepth recursives prec@(Precedence (sig, l)) = St.execState (mapM_ recdepthM syms) Map.empty
    where 
      ecss = eclasses prec
      
      eclassOf f = 
        case find (\ cs -> f `Set.member` cs) ecss of 
          Nothing -> Set.singleton f
          Just cs -> cs
          
      syms = Set.toList $ symbols sig        

      below f = Set.toList $ Set.unions [ eclassOf h | f' :>: h <- l , f == f' ]

      recdepthM f = do 
        m <- St.get
        case Map.lookup f m of 
          Just rd -> return rd
          Nothing -> do 
            rds <- mapM recdepthM (below f)
            let rd | f `Set.member` recursives = 1 + maximum (0:rds)
                   | otherwise                 = maximum (0:rds)
            St.modify (Map.insert f rd)
            return rd
        
      
      
      
          
      
                                                      
ranks :: Precedence -> Map.Map Symbol Int
-- | ranks of function symbols in precedence, starting at '1'
ranks prec@(Precedence(sig,_)) = recursionDepth (symbols sig) prec
