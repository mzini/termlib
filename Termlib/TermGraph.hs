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

module Termlib.TermGraph where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.List as List
import Data.List ((\\))
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

import Termlib.Trs (Trs)
import qualified Termlib.Trs as Trs
import qualified Termlib.Rule as Rule

-- import qualified Data.Graph.Inductive.Graph as Graph
-- import Data.Graph.Inductive.PatriciaTree (Gr)
-- import qualified Data.GraphViz as GraphViz
-- import Data.GraphViz.Attributes (Attribute(..), Shape(..), Label(..))

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
                           } deriving (Eq, Ord, Show)

nodes :: TermGraph -> Set Node
nodes g = root g `Set.insert` Set.fromList [ n | e <- Map.elems $ edges g, 
                                                      n <- attached e]


data St a = St { content :: a
               , newId :: Int}

freshNode :: State (St a) Node
freshNode = do s <- State.get
               let i = newId s
               State.put s{newId = i + 1}
               return $ mkNode i

fromTermM :: Term -> State (St (Map Variable Node)) TermGraph
-- identifies variables
fromTermM (Var x) = do n <- getTarget x 
                       return $ TermGraph n (Map.singleton n $ mkVariable n x)
    where getTarget v = do s <- State.get
                           case Map.lookup v (content s) of
                             Just n -> return n
                             Nothing -> fresh s
              where fresh (St m i) = do let n = mkNode (i + 1)
                                        State.put $ St (Map.insert v n m) (i+1)
                                        return n

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
              where e = fromMaybe (error $ "TermGraph.toTerm: graph not well-formed. no edge at node " ++ show r) $ edge r g

garbageCollect :: TermGraph -> TermGraph
garbageCollect g = g {edges = gc (root g)}
    where gc n = Map.insert n e $ Map.unions [gc m | m <- sources e]
              where e = fromMaybe (error $ "TermGraph.garbageCollect: graph not well-formed. no edge at node " ++ show n) $  edge n g


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

children :: TermGraph -> Node -> Maybe (Set Node)
children g n = do e <- edge n g 
                  return $ Set.fromList $ sources e

-- edgeAt :: TermGraph -> Position -> Maybe Edge
-- edgeAt g p = rootedEdge `liftM` subgraphAt g p


-- nodeAt :: TermGraph -> Position -> Maybe Node
-- nodeAt g p = root `liftM` subgraphAt g p



data TermGraphRule = TGR {lhs :: TermGraph, rhs :: TermGraph}

newtype TGS = TGS {rules :: [TermGraphRule]}

type Match = Map Variable Node

fromTrs :: Trs -> TGS
fromTrs trs = TGS [ TGR (fromTerm $ Rule.lhs r) (fromTerm $ Rule.rhs r) 
                    | r <- Trs.rules trs]

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
freshId :: TermGraph -> Int
freshId tg = 1 + foldl max 0 [nodeId n | n <- Set.toList $ nodes tg] 

replace :: (TermGraph, Match) ->  (TermGraph, Node) -> TermGraph
replace (new, match) (tg, node) = garbageCollect $ tg {edges = Map.delete node (edges tg) `Map.union` edges'}
    where root'     = fromMaybe (error "TermGraph.replace") $ Map.lookup (root new) m 
          (edges',(St m _)) = State.runState wireNew (St (Map.singleton (root new) node) (freshId tg))

          wireNew  = foldM 
                     (\ es (n,e) -> 
                          case label e of
                            VL x -> return es
                            FL f -> do n' <- nodeOf n
                                       sources' <- mapM nodeOf (sources e)
                                       return $ Map.insert n' e{target=n',  sources=sources'} es) 
                     Map.empty $ 
                     Map.toList (edges new)


          nodeOf n = case label `liftM` edge n new of 
                       Just (VL v) -> return $ fromMaybe (error "TermGraph.replace") $ Map.lookup v match
                       _           -> do s@(St m i) <- State.get
                                         case Map.lookup n m of 
                                           Just n' -> return n'
                                           Nothing -> do let n' = mkNode i
                                                         State.put s{content = Map.insert n n' m, newId = i + 1}
                                                         return $ n'

step :: TGS -> Strategy -> TermGraph -> Maybe TermGraph
step tgs strat tg =
    do redexNode <- strat tg
       (rhs, match) <- msum [ (,) (rhs rule) `liftM` match (lhs rule) (tg,redexNode) | rule <- rules tgs]
       return $ replace (rhs, match) (tg,redexNode)
                        

termEq :: TermGraph -> Node -> Node -> Bool
-- Pointer equality
termEq g n1 n2 = fromMaybe (error $ "TermGraph.termEq " ++ show n1 ++ show n2) comp
    where comp = do e1 <- edge n1 g
                    e2 <- edge n2 g
                    return $ label e1 == label e2 && sources e1 == sources e2


rename :: TermGraph -> Node -> Node -> TermGraph
rename tg node newNode = TermGraph (maybeReplace $ root tg) edges'
    where maybeReplace n | n == node = newNode 
                         | otherwise = n
          edges' = Map.foldWithKey f Map.empty (edges tg) 
          f n e = Map.insert 
                    (maybeReplace n) 
                    e{target = maybeReplace (target e)
                     , sources = map maybeReplace (sources e)}


nfNormalize :: TGS -> TermGraph -> TermGraph
nfNormalize tgs tg = step tg $ Set.toList $ nfNodes $ root tg
    where step g nfs | prs == [] = g 
                     | otherwise = step (foldl mergeNodes g prs) (nfs \\ [ n | (n,_) <- prs])
              where prs = [ (n1, n2) | 
                            n1 <- nfs, 
                            n2 <- nfs, 
                            n1 /= n2, 
                            termEq g n1 n2]
                    mergeNodes g' = uncurry $ rename g'
          nfNodes n = case subgraphAt tg n of 
                        Just sg -> if isRedex tgs sg 
                                  then Set.unions $ map nfNodes $ 
                                       Set.elems (fromMaybe (error "TermGraph.nfNormalize.children") $
                                                  children tg n)
                                  else nodes sg
                        Nothing -> error $ "TermGraph.nfNormalize.nfNodes" ++ show n
                             

innermostRewriteStep :: TGS -> TermGraph -> Maybe TermGraph
innermostRewriteStep tgs g = step tgs (innermost tgs) $ nfNormalize tgs g


-- data GNodeLabel = N
--                 | VN String
--                 | FN String

-- type GEdgeLabel = Maybe Int

-- toGraph :: Fun.Signature -> TermGraph -> Gr GNodeLabel GEdgeLabel
-- toGraph sig tg = Graph.mkGraph ns es
--     where ns = [ (nodeId n , N) | n <-  Map.keys $ edges tg]
--                ++ [(i, mkl $ label e) | (i,e) <- indexedEdges]
--           es = concat [ mke (nodeId $ target e) i Nothing : [mke i (nodeId s) (Just j) | (s,j) <- zip (sources e) [1..]] 
--                         | (i,e) <- indexedEdges ]

--           mkl (VL l) = VN $ show l
--           mkl (FL f) = FN $ Fun.symbolName sig f
--           mke from to l = (from,to,l)
--           indexedEdges = zip [(1 + freshId tg) ..] (Map.elems $ edges tg)

-- toDot :: Fun.Signature -> TermGraph -> GraphViz.DotGraph
-- toDot sig tg = GraphViz.graphToDot (toGraph sig tg) [] nattrs eattrs
--     where nattrs (_,N) = [Shape Circle]
--           nattrs (_,VN l) = [Shape Circle, Label $ StrLabel l] 
--           nattrs (_,FN l) = [Shape BoxShape, Label $ StrLabel l] 
--           eattrs (_,_,Nothing) = []
--           eattrs (_,_,Just i)  = [Label $ StrLabel $ show i]
