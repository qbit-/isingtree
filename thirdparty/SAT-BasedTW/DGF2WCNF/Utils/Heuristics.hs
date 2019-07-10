module Heuristics (upperBoundTWGreedyDegree, upperBoundTWGreedyFillIn, getLowerBoundOnTW)  where

import qualified Data.Graph.Inductive as G
import qualified Data.Vector as V
import qualified Data.Set as S

import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Reader.Class


import Data.Maybe 
import Data.List (nub, minimumBy)
import DIMACParser
import Control.Applicative 
import Data.Ord (comparing)

{- 
  Module which implements heuristics for finding the treewidth of a graph, state is current LB 
-}

type GraphST a = State (G.UGr, Int) a

type UBCalculation a = ReaderT (V.Vector G.Node) (State (G.UGr, Int)) a

type ElimOrderingCalculation a = State G.UGr a

upperBoundTWGreedyDegree = findUpperBoundOnTW greedyDegree
upperBoundTWGreedyFillIn = findUpperBoundOnTW greedyFillIn

findUpperBoundOnTW :: (G.UGr -> G.Node) -> G.UGr -> Int  
findUpperBoundOnTW criterion graph = 
  snd $ execState (runReaderT fillGraphAndGetUB elimOrdering) (graph, 0)
 where
  elimOrdering = evalState (calculateAndEliminationOrdering criterion) graph

calculateAndEliminationOrdering :: (G.UGr -> G.Node) -> ElimOrderingCalculation (V.Vector G.Node) 
calculateAndEliminationOrdering criterion = 
  (gets G.noNodes) >>= (fmap V.fromList <$> flip replicateM choseNextNode)
 where
  choseNextNode = (criterion <$> get) >>= (\n -> const n <$> modify (eliminateNode n))

eliminateNode :: G.Node -> (G.UGr -> G.UGr)
eliminateNode node graph = G.insEdges edgesToAdd . G.delNode node $ graph
 where 
   n =  G.neighbors graph node 
   setOfEdges = S.fromList . G.edges $ graph
   edgesToAdd = map (\(p1, p2) -> (p1, p2, ())) . filter (`S.notMember` setOfEdges) $ [(x,y) | x <- n, y <- n, x < y] 

fillGraphAndGetUB :: UBCalculation ()
fillGraphAndGetUB = asks V.length >>= mapM_ processNode . enumFromTo 0 . subtract 1   

processNode :: Int -> UBCalculation ()
processNode index = 
  do
    (graph, curUb) <- get
    elimOrdering <- ask
    let   
      restOfNodes = V.drop index elimOrdering
      curEdges = S.fromList . G.edges $ graph
      curNode = elimOrdering V.! index
      neighs = filter (\n -> n `V.elem` restOfNodes) . G.neighbors graph $ curNode
      newEdges = map (\(p1, p2) -> (p1, p2, ())) . filter (`S.notMember` curEdges) $ [(x,y) | x <- neighs, y <- neighs, x < y]
    modify (\(g, lb) -> (G.insEdges newEdges g, max curUb (length neighs))) 
      
{- Criterions -}

greedyDegree :: (G.UGr -> G.Node)
greedyDegree g = minimumBy (comparing (G.deg g)) . G.nodes $ g

greedyFillIn :: (G.UGr -> G.Node)
greedyFillIn g = minimumBy (comparing (nonAdjacenNeighs)) . G.nodes $ g
 where
  curEdges = S.fromList . G.edges $ g
  nonAdjacenNeighs n = length .  filter (`S.notMember` curEdges) $ [(x, y) | x <- G.neighbors g n, y <- G.neighbors g n, x < y] 
 
{- LOWER BOUND -}
getLowerBoundOnTW :: G.UGr -> Int 
getLowerBoundOnTW g = evalState calculateLowerBound (g, 0) 

calculateLowerBound :: GraphST Int 
calculateLowerBound =
  do 
    (graph, curLb) <- get 
    if (G.noNodes graph <= 1) 
    then return curLb 
    else (max curLb) <$> 
          (modify (\(g, l) -> (filterZeroDegNodes g, l)) *> 
            (gets (findNextEdgeToRemove . fst) >>= contractEdge) *> 
             calculateLowerBound)  

contractEdge :: G.Edge -> GraphST ()
contractEdge e = modify (\(g, l) -> (contractEdge' e g, G.deg g (fst e)))

contractEdge' :: G.Edge -> G.UGr -> G.UGr
contractEdge' (x, y) graph = G.insEdges edgesToAdd . G.delNode p2 $ graph
 where
   p1 = min x y
   p2 = max x y
   neighbors = G.neighbors graph p2
   setOfEdges = S.fromList . G.edges $ graph
   edgesToAdd = map (\(p1, p2) -> (p1, p2, ())) . filter (\e -> e `S.notMember` setOfEdges) $ [(min p1 x, max p1 x) | x<-neighbors, x /= p1] 

filterZeroDegNodes :: G.UGr -> G.UGr 
filterZeroDegNodes graph = G.delNodes zeroDegs graph
 where
  zeroDegs  = filter ((== 0) . G.deg graph) . G.nodes $ graph

findNextEdgeToRemove :: G.UGr -> G.Edge
findNextEdgeToRemove graph = (minDegNode, minDegNeighbour)
 where 
   minDegreeInGraph = minimumBy (comparing (G.deg graph))
   minDegNode = minDegreeInGraph . G.nodes $ graph 
   context = fromJust . fst . G.match minDegNode $ graph
   minDegNeighbour = minDegreeInGraph . filter (/= minDegNode) . G.neighbors' $ context 
