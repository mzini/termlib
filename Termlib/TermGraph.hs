module Termlib.TermGraph where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.List as List
import Data.Maybe(fromMaybe)
import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State)
import Control.Monad (liftM, foldM, msum)

import qualified Termlib.Term as Term
import Termlib.Term (Term(..))
import qualified Termlib.Variable as Var
import Termlib.Variable (Variable)
import qualified Termlib.FunctionSymbol as Fun
import Termlib.FunctionSymbol (Symbol)


data EdgeLabel = VL Variable 
                    | FL Symbol deriving (Eq, Ord, Show)

newtype Node = Node {nodeId :: Int} deriving (Eq, Ord, Show)

data Edge = Edge { target  :: Node
                 , sources :: [Node]
                 , label   :: EdgeLabel
                 } deriving (Eq, Ord, Show)

mkNode :: Int -> Node
mkNode = Node

mkEdge :: Node -> [Node] -> EdgeLabel -> Edge
mkEdge = Edge

mkVariable :: Node -> Variable -> Edge
mkVariable r v = mkEdge r [] $ VL v

mkFun :: Node -> [Node] -> Symbol -> Edge
mkFun r cs v = mkEdge r cs $ FL v

attached :: Edge -> [Node]
attached e =  target e : sources e

isVariable :: TermGraph -> Node -> Bool
isVariable g n = case label `liftM` edge n g of 
                   Just (VL x) -> True
                   _           -> False

isFun :: TermGraph -> Node -> Bool
isFun g n = case label `liftM` edge n g of 
              Just (FL x) -> True
              _           -> False

isDangling :: TermGraph -> Node -> Bool
isDangling g n = case label `liftM` edge n g of 
                   Just _ -> False
                   _      -> True

data TermGraph = TermGraph {root :: Node
                           , edges :: Map Node Edge
                           } 

nodes :: TermGraph -> Set Node
nodes g = root g `Set.insert` Set.fromList [ n | e <- Map.elems $ edges g, 
                                                      n <- attached e]


data St a = St { content :: a
               , newId :: Int}

getTarget :: Variable -> State (St (Map Variable Node)) Node
getTarget v = do s <- State.get
                 case Map.lookup v (content s) of
                   Just n -> return n
                   Nothing -> fresh s
    where fresh (St m i) = do let n = mkNode (i + 1)
                              State.put $ St (Map.insert v n m) (i+1)
                              return n

freshNode :: State (St a) Node
freshNode = do s <- State.get
               let i = newId s
               State.put s{newId = i + 1}
               return $ mkNode i

fromTermM :: Term -> State (St (Map Variable Node)) TermGraph
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

edge :: Node -> TermGraph -> Maybe Edge
edge r g = Map.lookup r $ edges g

toTerm :: TermGraph -> Term
toTerm g = toTerm' (root g)
    where toTerm' r = case label e of
                        VL x -> Var x
                        FL f -> Fun f $ map toTerm' (sources e)
              where e = fromMaybe (error "TermGraph.toTerm") $ edge r g

garbageCollect :: TermGraph -> TermGraph
garbageCollect g = g {edges = gc (root g)}
    where gc n = Map.insert n e $ Map.unions [gc m | m <- sources e]
              where e = fromMaybe (error "TermGraph.toTerm") $  edge n g


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
directSubgraphs g = map mk (sources $ rootedEdge g)
    where mk n = TermGraph n (edges g)

subgraphAt :: TermGraph -> Node -> Maybe TermGraph
subgraphAt g n = case Map.member n (edges g) of 
                   True -> Just g{root = n}
                   False -> Nothing


rootedEdge :: TermGraph -> Edge
rootedEdge g = fromMaybe (error "TermGraph.rootedEdge") $ edge (root g) g


-- edgeAt :: TermGraph -> Position -> Maybe Edge
-- edgeAt g p = rootedEdge `liftM` subgraphAt g p


-- nodeAt :: TermGraph -> Position -> Maybe Node
-- nodeAt g p = root `liftM` subgraphAt g p



data TermGraphRule = TGR {lhs :: TermGraph, rhs :: TermGraph}

newtype TGS = TGS {rules :: [TermGraphRule]}

type Match = Map Variable Node

match :: TermGraph -> (TermGraph, Node) -> Maybe Match
match pattern (graph,node) = do redex <- subgraphAt graph node
                                matchRoot Map.empty (redex,pattern)
    where matchRoot m (g,p) = 
              case label $ rootedEdge p of 
                VL v -> case Map.lookup v m of 
                          Just n  -> if n == (root g) 
                                     then Just m
                                     else Nothing
                          Nothing -> Just $ Map.insert v (root g) m

                FL f -> if rootedEdge g `eEq` rootedEdge p
                        then foldM matchRoot m $ zip (directSubgraphs g) (directSubgraphs p)
                        else Nothing
                    where eEq e1 e2 = label e1 == label e2
                                      && length (sources e1) == length (sources e2)


matches :: TermGraph -> (TermGraph,Node) -> Bool
matches pattern m = case match pattern m of 
                            Just _ -> True
                            _      -> False

isRedex :: TGS -> TermGraph -> Bool
isRedex tgs graph = any (\ lhs -> matches lhs (graph,root graph)) [lhs r | r <- rules tgs]


type Strategy = TermGraph -> Maybe Node

innermost :: TGS -> Strategy 
innermost tgs g = case msum [innermost tgs gi | gi <- directSubgraphs g, 
                             c <- List.nub $ sources $ rootedEdge gi ] of
                Nothing -> if isRedex tgs g
                           then Just (root g)
                           else Nothing
                a       -> a


replace :: (TermGraph, Match) ->  (TermGraph, Node) -> TermGraph
replace (new, match) (tg, node) = garbageCollect $ tg {edges = Map.delete node (edges tg) `Map.union` edges'}
    where root'     = fromMaybe (error "TermGraph.replace") $ Map.lookup (root new) m 

          (edges',(St m _)) = State.runState wireNew (St (Map.singleton (root new) node) freshId)
                    
          wireNew  = foldM 
                     (\ es (n,e) -> 
                          case label e of
                            VL x -> return es
                            FL f -> do n' <- nodeOf n
                                       sources' <- mapM nodeOf (sources e)
                                       return $ Map.insert n' e{target=n',  sources=sources'} es) 
                     Map.empty $ 
                     Map.toList (edges new)

          freshId   = 1 + foldl max 0 [nodeId n | n <- Set.toList $ nodes tg] 
          nodeOf n = case label `liftM` edge n new of 
                       Just (VL v) -> return $ fromMaybe (error "TermGraph.replace") $ Map.lookup v match
                       _           -> do s@(St m i) <- State.get
                                         case Map.lookup n m of 
                                           Just n' -> return n'
                                           Nothing -> do let n' = mkNode i
                                                         State.put s{content = Map.insert n n' m, newId = i + 1}
                                                         return $ n'

stp :: TGS -> Strategy -> TermGraph -> Maybe TermGraph
stp tgs strat tg =
    do redexNode <- strat tg
       (rhs, match) <- msum [ (,) (rhs rule) `liftM` match (lhs rule) (tg,redexNode) | rule <- rules tgs]
       return $ replace (rhs, match) (tg,redexNode)
                        
                  