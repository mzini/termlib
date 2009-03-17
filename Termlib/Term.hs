module Termlib.Term
where

import qualified Termlib.Variable as V
import Termlib.Variable (Variable)
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Symbol)
import Termlib.Utils
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State.Lazy as State

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Term = Var Variable | Fun F.Symbol [Term]
  deriving (Eq, Ord, Show)


variables :: Term -> Set Variable
variables (Var v) = Set.singleton v
variables (Fun _ xs) = Set.unions $ [variables x | x <- xs]

functionSymbols :: Term -> Set Symbol 
functionSymbols (Var _) = Set.empty
functionSymbols (Fun f xs) = Set.insert f $ Set.unions $ [functionSymbols x | x <- xs]

depth (Var _) = 0
depth (Fun _ []) = 0
depth (Fun _ xs) = (succ . maximum . map depth) xs

root (Var v) = Left v
root (Fun f _) = Right f

size :: Term -> Int
size (Var _) = 1
size (Fun _ xs) = (succ . sum . map size) xs

fsize :: Term -> Int
fsize (Var _) = 0
fsize (Fun _ xs) = (succ . sum . map fsize) xs

varDepth :: Term -> Maybe Int
varDepth (Var _) = Just 0
varDepth (Fun _ xs) = if subresults == [] then Nothing else (Just . succ . maximum) subresults
  where subresults = (Maybe.catMaybes . map varDepth) xs

isFlat :: Term -> Bool
isFlat t = depth t <= 1

isShallow :: Term -> Bool
isShallow = maybe True (<= 1) . varDepth

isLinear :: Term -> Bool
isLinear t = fst $ State.runState (l t) Set.empty 
  where l (Var x)    = do s <- State.get
                          return $ x `Set.notMember` s
        l (Fun _ ts) = foldM (\ b t_i -> (b &&) `liftM` l t_i) True ts 
  
isGround :: Term -> Bool
isGround = Set.null . variables

immediateSubterms :: Term -> [Term]
immediateSubterms (Var _) = []
immediateSubterms (Fun _ xs) = xs

isSubterm :: Term -> Term -> Bool
s `isSubterm` t = s == t || (any (s `isSubterm`) . immediateSubterms) t

isSuperterm :: Term -> Term -> Bool
isSuperterm = flip isSubterm

isVariable :: Term -> Bool
isVariable (Var _) = True
isVariable (Fun _ _) = False


varCardinality :: Variable -> Term -> Int
varCardinality x (Var y) | x == y    = 1
                         | otherwise = 0
varCardinality x (Fun _ xs) = (sum . map (varCardinality x)) xs


canonise (Var x) varmap = case Map.lookup x varmap of
  Nothing -> addvar x varmap
    where addvar v vm = let newelem = freshvar varmap in
            (Var newelem, Map.insert v newelem vm)
          freshvar = invEnum . (1 +) . maximum . (0 :) . map enum . Map.keys
  Just oldelem -> (Var oldelem, varmap)
canonise (Fun f xs) varmap = (Fun f (fst subresult), snd subresult)
  where subresult = foldl doSubTerm ([], varmap) xs
        doSubTerm a t = (fst a ++ [fst subsubresult], snd subsubresult)
          where subsubresult = canonise t (snd a)
