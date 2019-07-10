module VariableEnumeration (createVariableEnumerations, TWVariables(..)) where 

import PropLogic
import Data.Maybe
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive as G
import TWTypes
import CNFTypes
---------------------

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Reader.Class
import Control.Monad.State.Class
--------------------



data TWVariables =
 TWVariables 
  {
   -- Added for all i < j
   widthOrderF ::  NodePairF,
   -- Added for all i /= j
   arcInTriangulation :: NodePairF,
   maxWeight :: Weight,
   totNodes :: Node, 
   edges :: [G.Edge]
  }
instance ProblemInfo TWVariables where
  hardClauseW = maxWeight
  highestV = getHighestUsedVariable
  ttP = totNodes

getHighestUsedVariable :: TWVariables -> Node 
getHighestUsedVariable b = 
  variableNumber $ (arcInTriangulation b) (totNodes b) ((subtract 1) . totNodes $ b) 

{-
data TWVariablesBIT =
 TWVariablesBIT 
  {
   -- Added for all i /= j
   arcInTriangulation :: NodePairF,
   bitVariables :: BitF,
   -- Added for all i < j
   bitEqualityF :: BitPairF,
   gtVars :: NodePairF,
   maxWeight :: Weight,
   totNodes :: Node, 
   edges :: [Edge]
  }
instance ProblemInfo TWVariablesBIT where
  hardClauseW = maxWeight
  highestV = getHighestUsedVariableBIT
  ttP = totNodes

getHighestUsedVariableBIT :: TWVariablesBIT -> Node 
getHighestUsedVariableBIT b = 
  variableNumber $ (gtVars b) ((subtract 1) . totNodes $ b) (totNodes b) 
-}


---------------------------STATE VERSION------------------------------
type EnumerationCreator a = StateT Int (Reader G.UGr) a

{-
  The environment of the reader is the total nodes
  The state of the... state, is the current offset (maximum of the last function) 
  Assumes that all variables have some form of parent sets possibilities
-}
createVariableEnumerations :: G.UGr -> TWVariables
createVariableEnumerations = runReader stateC
 where
  stateC = evalStateT tWEnum 0


tWEnum :: EnumerationCreator TWVariables
tWEnum =  
  do
    widthOrdF <- undirectedNodePairs
    arcF <- directedNodePairs 
    totalNodes <- asks G.noNodes
    e <- asks G.edges 
    return $
      TWVariables 
        {
          -- Added for all i < j
          widthOrderF = widthOrdF,
          -- Added for all i /= j
          arcInTriangulation = arcF,
          -- Added for all i < j
          maxWeight = fromIntegral totalNodes,
          totNodes = totalNodes,
          edges = e 
        }

--Undirected means that the function only accepts p1 < p2
undirectedNodePairs :: EnumerationCreator NodePairF
undirectedNodePairs = nodePairF findNumberOfUndirectedPairVariable 
 
--The Function Should accept all cases where i /= j  
directedNodePairs :: EnumerationCreator NodePairF
directedNodePairs = nodePairF findNumberOfDirectedPairVariable


--Generic function to abstract the similarities of directed and undirected pairs
nodePairF :: NodePairNumberingF -> EnumerationCreator NodePairF
nodePairF function = 
  do 
    totalNodes <- asks G.noNodes
    offset <- get
    let 
      nodePairFunction p1 p2 = NamingVariable . (+offset) $ function totalNodes p1 p2
    put . variableNumber $ nodePairFunction totalNodes (totalNodes - 1)
    return nodePairFunction

-- Bit functions to get the bit variables for each point. 
bitVariableF :: EnumerationCreator BitF
bitVariableF =
  do 
    totalNodes <- asks G.noNodes
    offset <- get 
    let  
      totBits = calculateLog $ totalNodes
      bitF b p1 = NamingVariable . (+offset) $ (p1 - 1) * totBits + b 
    put . variableNumber $ bitF totBits totalNodes
    return bitF
 
--Function for getting the equality variables of two points and a bit (undirected pairs of points)
bitPairVariableF :: EnumerationCreator BitPairF
bitPairVariableF =
  do
    totalNodes <- asks G.noNodes
    offset <- get 
    let
      totBits = calculateLog $ totalNodes 
      equalBitsF b p1 p2 = NamingVariable . (+offset) $ findNumberOfBitEqualityVariables totalNodes b p1 p2
    put . variableNumber $ equalBitsF totBits (totalNodes - 1) totalNodes   
    return equalBitsF

--------------------------
 
--Variables added only when p1 < p2, Note that these return variable numbers, not indexes.   
type NodePairNumberingF = Int -> Node -> Node -> Int

findNumberOfUndirectedPairVariable :: NodePairNumberingF
findNumberOfUndirectedPairVariable tot p1 p2 
  | p1 == p2 = undefined
  | p1 < p2 = (+dif) . firstVarNumForPoint tot $ p1
  | otherwise =  findNumberOfUndirectedPairVariable tot p2 p1
 where
  dif = p2 - p1 - 1 

firstVarNumForPoint :: Int -> Int -> Int 
firstVarNumForPoint _ 1 = 1
firstVarNumForPoint tp p = (+1) . lastVarNumForPoint tp $ (p-1) 

lastVarNumForPoint :: Int -> Int -> Int 
lastVarNumForPoint tp p =
  let
    sumToP = p*(p+1) `div` 2
    firstProduct = tp * p
  in firstProduct - sumToP 

--For variables added whenever x /= y, also returns actual variable numbers
findNumberOfDirectedPairVariable ::  NodePairNumberingF
findNumberOfDirectedPairVariable tot p1 p2
  | p1 == p2 = undefined
  | otherwise = (p1 -1) * (tot - 1) + dif
 where
  dif = if p1 < p2 then p2 - 1 else p2

--Variable numbers for three parameter variables (Nodes + bit or Nodes + Common child)
-- Here i < j
findNumberOfBitEqualityVariables :: Int -> Bit -> Node -> Node -> Int
findNumberOfBitEqualityVariables totNodes bit p1 p2 = findNumberOfThreeParameterNodes totNodes (calculateLog totNodes) bit p1 p2
        
findNumberOfThreeParameterNodes :: Int -> Int -> Int -> Node -> Node -> Int
findNumberOfThreeParameterNodes totNodes maxThird thirdParam p1 p2 
  | p1 == p2 = undefined
  | p1 < p2 =  (+thirdParam) . (*maxThird) . subtract 1 . findNumberOfUndirectedPairVariable totNodes p1 $ p2
  | otherwise =  findNumberOfThreeParameterNodes totNodes maxThird thirdParam p2 p1

calculateLog :: Int -> Int
calculateLog 1 = 1
calculateLog x = ceiling . logBase 2 . fromIntegral $ x




