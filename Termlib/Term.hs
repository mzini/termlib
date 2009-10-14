module Termlib.Term
where

import qualified Termlib.Variable as V
import Termlib.Variable (Variable(..), canonical)
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
                          if x `Set.member` s 
                           then return False
                           else State.put (Set.insert x s) >> return True
        l (Fun _ ts) = foldM (\ b t_i -> (b &&) `liftM` l t_i) True ts 
  
isGround :: Term -> Bool
isGround = Set.null . variables

immediateSubterms :: Term -> [Term]
immediateSubterms (Var _) = []
immediateSubterms (Fun _ xs) = xs

nonVariableSubterms :: Term -> [Term]
nonVariableSubterms = genSubterms False

properNonVariableSubterms :: Term -> [Term]
properNonVariableSubterms = genProperSubterms False

subterms :: Term -> [Term]
subterms = genSubterms True

properSubterms :: Term -> [Term]
properSubterms = genProperSubterms True

genSubterms :: Bool -> Term -> [Term]
genSubterms includeVars t@(Var _) = if includeVars then [t] else []
genSubterms iv t@(Fun _ ts)       = t : (concat $ map (genSubterms iv) ts)

genProperSubterms :: Bool -> Term -> [Term]
genProperSubterms iv = concat . map (genSubterms iv) . immediateSubterms

isProperSubtermOf :: Term -> Term -> Bool
s `isProperSubtermOf` t = any (s `isSubtermOf`) $ immediateSubterms t

isSubtermOf :: Term -> Term -> Bool
s `isSubtermOf` t = s == t || s `isProperSubtermOf` t

isSupertermOf :: Term -> Term -> Bool
isSupertermOf = flip isSubtermOf

isProperSupertermOf :: Term -> Term -> Bool
isProperSupertermOf = flip isProperSubtermOf

isVariable :: Term -> Bool
isVariable (Var _) = True
isVariable (Fun _ _) = False


cardinality :: Either Variable Symbol -> Term -> Int
cardinality x (Var y) | x == (Left y)    = 1
                      | otherwise        = 0
cardinality x (Fun f xs) = (sum . map (cardinality x)) xs + if x == (Right f) then 1 else 0

varCardinality :: Variable -> Term -> Int
varCardinality v = cardinality (Left v)

funCardinality :: Symbol -> Term -> Int
funCardinality f = cardinality (Right f)

type Renaming = Map.Map Variable Variable
canonise :: Term -> Renaming -> (Term, Renaming)
canonise (Var x) varmap = 
    case Map.lookup x varmap of
      Nothing -> (Var freshVar, Map.insert x freshVar varmap)
          where freshVar = canonical $ 1 + Map.fold (\ (Canon i) n -> max i n) 0 varmap
      Just oldelem -> (Var oldelem, varmap)

canonise (Fun f xs) varmap = (Fun f (fst subresult), snd subresult)
    where subresult = foldl doSubTerm ([], varmap) xs
          doSubTerm a t = (fst a ++ [fst subsubresult], snd subsubresult)
              where subsubresult = canonise t (snd a)