{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Termlib.Types 
    (
     Type
    , TypeDecl (..)
    , (:::) (..)
    , Typing (..)
    , infer
    , types 
    , equivs
    , decls
    , partition
    , restrictTo
    )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.RWS
import Text.PrettyPrint.HughesPJ hiding (empty)

import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import qualified Termlib.Substitution as Subst
import qualified Termlib.Signature as Sig
import Termlib.Term
import qualified Termlib.Rule as R
import Termlib.Utils 

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.DFS as GraphDFS
import qualified Data.Graph.Inductive.Tree as GraphT


data a ::: b = a :::b 

type Type a = a

data TypeDecl a = TypeDecl { inputTypes :: [Type a] 
                           , outputType :: Type a }
type Typing a = Map.Map F.Symbol (TypeDecl a)


type UP = [(Type Int, Type Int)]

newtype InferM a = 
  InferM { run :: RWS () UP Int a }
  deriving (Monad, MonadWriter UP, MonadState Int)

(=~) :: Type Int -> Type Int -> InferM ()
a =~ b = tell [(a,b)]

infer :: F.Signature -> [R.Rule] -> Typing String
infer sig rs = rename $ instDecl id unifier `Map.map` decs
    where 
      rename :: Typing Int -> Typing String
      rename ds = instDecl (error "rename decl") renamer `Map.map` ds
          where 
            vs = snub $ concatMap (\ (TypeDecl its ot) -> ot : its) (Map.elems ds)
            names =  (\ c -> [c]) `map` (['a'..'z'] ++ ['A'..'Z'])
                     ++ [show i | i <- [1..]]
            renamer = Map.fromList $ zip vs names

      instDecl mk subst (TypeDecl its ot) = TypeDecl [apply' mk subst t | t <- its] (apply' mk subst ot)

      (decs,_,up) = runRWS (run inferM) () 0

      mkDecl at (sym,attribs) = do 
        its <- mapM (const fresh) [1.. F.symArity attribs]
        ot <- fresh
        return $ Map.insert sym (TypeDecl its ot) at

      inferM = do 
        decls <- foldM mkDecl Map.empty $ Sig.toList sig
        typeRule decls `mapM` rs
        return decls

      fresh = do { i <- get; put (i + 1); return i}

      typeRule decls rl = do 
        let vars = Set.toList (R.variables rl)
        env <- foldM (\ e v -> flip (Map.insert v) e `liftM` fresh) Map.empty vars
        l <- typeTerm decls env (R.lhs rl)
        r <- typeTerm decls env (R.rhs rl)
        l =~ r

      typeTerm _ env (Var v) = return $ lookupEnv env v
      typeTerm decls env (Fun f ts) = do 
                 its' <- typeTerm decls env `mapM` ts
                 sequence_ [ t1 =~ t2 | (t1,t2) <- zip its' its ]
                 return ot
         where TypeDecl its ot = lookupDecl decls f


      lookupEnv env v = fromJust (Map.lookup v env)
      lookupDecl decls f = fromJust (Map.lookup f decls)

      unifier = unify up
          where 
            empty = Map.empty
            unify [] = empty
            unify ((t1,t2):ts)
                | t1 == t2 = unify ts
                | otherwise = s `compose` unify [(s `apply` t3,s `apply` t4) | (t3,t4) <- ts]
                where s = Map.insert t1 t2 empty
            -- f subst (t1,t2) = Map.fromList [(t1,t1),(t2,t1)] `compose` subst
            s1 `compose` s2 = (apply s2 `Map.map` s1) `Map.union` s2 -- left-biased

      apply = apply' id
      apply' mk  subst t = 
          case Map.lookup t subst of 
            Just t' -> t'
            Nothing -> mk t

types :: Ord a => Typing a -> Set.Set (Type a)
types = Map.fold (\ (TypeDecl its o) s -> Set.fromList (o:its) `Set.union` s) Set.empty

decls :: Typing a -> [F.Symbol ::: TypeDecl a]
decls typing = [ f ::: decl | (f,decl) <- Map.toList typing ]

-- | computes the equivalence of the transitive,reflexive closure of the input/output ordering 
-- @a_i >= a@ iff @f : a_1,...,a_k -> a@ for some @i `elem` [1,...,k]@
equivs :: Ord a => Typing a -> [[Type a]]
equivs (typing :: Typing a) = [ [fromJust (Graph.lab gr n) | n <- scc] | scc <- GraphDFS.scc gr]
    where 
      gr :: GraphT.Gr a ()
      gr = Graph.mkGraph ns es
      ns = zip [0..] (Set.toList (types typing))
      es = [ (i, i,()) | (i,_) <- ns ]
           ++ [ (theID to, theID ti,()) | _ ::: TypeDecl tis to <- decls typing 
                                        , ti <- tis]
      ids = Map.fromList [(n,i) | (i,n) <- ns]
      theID n = fromJust (Map.lookup n ids)


partition :: (F.Symbol -> TypeDecl a -> Bool) -> Typing a -> (Typing a,Typing a)
partition p = Map.mapEitherWithKey (\ f d -> if p f d then Left d else Right d)

restrictTo :: Set.Set F.Symbol -> Typing a -> Typing a
restrictTo fs = fst . partition (\ f _ -> f `Set.member` fs)

instance PrettyPrintable (TypeDecl (Type String)) where
    pprint (TypeDecl its os)
        | null its = ppType os
        | length its == 1 = ppis <+> text "->" <+> ppType os
        | otherwise = parens ppis <+> text "->" <+> ppType os
        where 
          ppis = hcat $ punctuate (text ",") [ppType i | i <- its]
          ppType = text 

instance PrettyPrintable (Typing (Type String), F.Signature) where
    pprint (typing, sig) 
        | Map.null typing = text "empty"
        | otherwise       =  vcat [pp f tp | (f,tp) <- Map.toList typing ]
        where pp f tp = pprint (f,sig) <+> text "::" <+> pprint tp
