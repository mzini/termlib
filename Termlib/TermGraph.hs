module Termlib.TermGraph where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State)


import qualified Termlib.Term as Term
import Termlib.Term (Term(..))
import qualified Termlib.Variable as Var
import Termlib.Variable (Variable)
import qualified Termlib.FunctionSymbol as Fun
import Termlib.FunctionSymbol (Symbol)

data HyperEdgeLabel = VL Variable 
                    | FL Symbol deriving (Eq, Ord, Show)

newtype HyperNode = HyperNode {nodeId :: Int} deriving (Eq, Ord, Show)

data HyperEdge = HyperEdge { target  :: HyperNode
                           , sources :: [HyperNode]
                           , label   :: HyperEdgeLabel
                           } deriving (Eq, Ord, Show)

mkNode :: Int -> HyperNode
mkNode = HyperNode

mkEdge :: HyperNode -> [HyperNode] -> HyperEdgeLabel -> HyperEdge
mkEdge = HyperEdge

mkVariable :: HyperNode -> Variable -> HyperEdge
mkVariable r v = mkEdge r [] $ VL v

mkFun :: HyperNode -> [HyperNode] -> Symbol -> HyperEdge
mkFun r cs v = mkEdge r cs $ FL v

attached :: HyperEdge -> [HyperNode]
attached e =  target e : sources e


data TermGraph = TermGraph {root :: HyperNode
                           , edges :: Map HyperNode HyperEdge
                           } 

nodes :: TermGraph -> Set HyperNode
nodes g = root g `Set.insert` Set.fromList [ n | e <- Map.elems $ edges g, 
                                                      n <- attached e]

data St = St { varNodes :: Map Variable HyperNode
             , newId :: Int}

getTarget :: Variable -> State St HyperNode
getTarget v = do s <- State.get
                 case Map.lookup v (varNodes s) of
                   Just n -> return n
                   Nothing -> fresh s
    where fresh (St m i) = do let n = mkNode (i + 1)
                              State.put $ St (Map.insert v n m) (i+1)
                              return n

freshNode :: State St HyperNode
freshNode = do s <- State.get
               let i = newId s
               State.put s{newId = i + 1}
               return $ mkNode i

fromTermM :: Term -> State St TermGraph
fromTermM (Var x) = do n <- getTarget x 
                       return $ TermGraph n (Map.singleton n $ mkVariable n x)

fromTermM (Fun f ts) = do n <- freshNode
                          gts <- mapM fromTermM ts
                          let roots = map root gts
                              edge  = mkFun n roots f
                              es    = Map.insert n edge $ Map.unions [edges gt | gt <- gts]
                          return $ TermGraph n es

fromTerm :: Term -> TermGraph
fromTerm t = fst $ State.runState (fromTermM t) (St Map.empty 0)

edge :: HyperNode -> TermGraph -> HyperEdge
edge r g = fromMaybe (error "TermGraph.edge") (Map.lookup r $ edges g)

toTerm :: TermGraph -> Term
toTerm g = toTerm' (root g)
    where toTerm' r = case label e of
                        VL x -> Var x
                        FL f -> Fun f $ map toTerm' (sources e)
              where e = edge r g

garbageCollect :: TermGraph -> TermGraph
garbageCollect g = g {edges = gc (root g)}
    where gc n = Map.insert n e $ Map.unions [gc m | m <- sources e]
              where e = edge n g


type Position = [Int]

rootPosition :: Position
rootPosition = []

fromList :: [Int] -> Position
fromList = id

conc :: Position -> Position -> Position
conc = (++)

prepend :: Int -> Position -> Position
prepend = (:)


directSubgraphs :: TermGraph -> [TermGraph]
directSubgraphs g = map mk (sources $ edge (root g) g)
    where mk n = TermGraph n (edges g)

subgraphAt :: TermGraph -> Position -> TermGraph
subgraphAt g []     = g
subgraphAt g (n:ns) = subgraphAt (g {root = src}) ns
    where src = (sources $ edge (root g) g)!!n

type Strategy = TermGraph -> Maybe Position

data TermGraphRule = TGR {lhs :: TermGraph, rhs :: TermGraph}

type TGS = [TermGraphRule]

innermost :: TGS -> Strategy 
innermost = undefined


type Match = Map Variable HyperNode

match :: TermGraph -> TermGraph -> Maybe Match
match g p = match' Map.empty g p 
    where match' m g p = case label $ edge (root p) p of 
                           VL v -> matchVar m g v
                           FL f -> matchFun m g f
          matchVar m g v = case Map.lookup v m of 
                             Just n  -> if n == (root g) 
                                        then Just m
                                        else Nothing
                             Nothing -> Just $ Map.insert v (root g) m
          matchFun m g f = if label e == FL f
                           then undefined
                           else undefined
              where e = edge (root g) g
          me e1 e2 = label e1 == label e2
                     && length (sources e1) == length (sources e2)

stp :: Strategy -> TermGraph -> Maybe TermGraph
stp = undefined
