module TreeWidthClauses (allTreewidthClausesSAT, allTreewidthClauses, InstSpecs(TWInstance)) where 

import VariableEnumeration
import CNFTypes
import CNFGeneration
import TWTypes 
import PropLogic
import CardinalityConstraints
import Debug.Trace
-----
import Data.List
import Control.Applicative 
import qualified Data.Vector as V
import Control.Monad

type TWCalc a = ClauseAdder TWVariables a

data InstSpecs = TWInstance Bool Bool Bool Bool

{- The middle one is there in order to simulate the original Paper encoding-}
allTreewidthClausesSAT :: Int -> Int -> InstSpecs -> TWCalc ()
allTreewidthClausesSAT lb ub i  = commonTreeWClauses i >> resolvedPropagateArcClauses >> conflictOnBigTreeWidthAllNodesSAT lb ub 
 
allTreewidthClauses :: Int -> Int -> InstSpecs -> TWCalc ()
allTreewidthClauses lb ub i  =  commonTreeWClauses i >>  conflictOnBigTreeWidthAllNodes lb ub

commonTreeWClauses :: InstSpecs -> TWCalc ()
commonTreeWClauses (TWInstance levelEncode resolveEdges resolveArcs breakArcCycles) = 
  do
    {- First part to Ensure that ord variables define a total order -}
    if levelEncode then levelEncodingOfTotalOrder else allTransitivityConstraints
    {- If we wish to resolve either edges or arcs we need the following clauses-}
    when (resolveEdges || resolveArcs) arcsOnlyTravelForvard    
    {- Add arcs for each edge in the input graph-}
    if resolveEdges then resolvedArcEdgeClauses else origArcEdgeClauses
    {- Propagate the arcs-}
    if resolveArcs then resolvedPropagateArcClauses else propagateAllArcs
    when breakArcCycles breakCyclesInArcs


{------------------------------------Level Encoding of Ord Variables------------------------------------------------------------------}
levelEncodingOfTotalOrder :: TWCalc ()
levelEncodingOfTotalOrder =
  do 
    f <- asksFromInfo widthOrderF  
    nodes <- asksFromInfo totNodes 
    levelEncodeTotalOrder nodes f 


levelEncodeTotalOrder :: Int -> (Int -> Int -> Variable) -> TWCalc ()
levelEncodeTotalOrder amNodes f = 
  do
   bitVars <- defineBitVars 
   mapM_ (\(p1,p2) -> encodeOrder (f p1 p2) (bitVars V.! (p1 -1)) (bitVars V.! (p2 -1))) [(x,y) | x<-allVars, y<-allVars, x < y]       
  where      
    allVars = [1..amNodes]
    totalBits = if amNodes == 1 then amNodes else ceiling . logBase 2 . fromIntegral $ amNodes 
    defineOneBitSet = (map NamingVariable . take totalBits . enumFrom) <$> (auxiliaryVariables totalBits) 
    defineBitVars = V.fromList <$> replicateM amNodes defineOneBitSet

encodeOrder :: Variable -> [Variable] -> [Variable] -> TWCalc ()
encodeOrder var p1bits p2bits =
  do 
    eqVars <- V.fromList <$> (sequence . zipWith (defineEq) p1bits) p2bits
    {-Total Order -}
    addHardClause . map (flip (,) False) . V.toList $ eqVars
    {- Define GT vars -}
    gtVars <- sequence . V.toList . V.imap (\i p1v -> defineGT (p2V V.! i) p1v (V.drop (i+1) eqVars)) $ p1V
    {- So now we add var true -> p1 < p2 -}
    addHardClause . ((var,False):) . map (flip (,) True) $ gtVars 
    {- To secure a total order we need the other direction -}
    mapM_ (\v -> addHardClause [(v, False), (var,True)]) gtVars
  where
   p1V = V.fromList p1bits
   p2V = V.fromList p2bits

defineEq :: Variable -> Variable -> TWCalc Variable
defineEq v1 v2 = 
  do 
    equalityVar <- NamingVariable <$> auxiliaryVariables 1
    let
      clausesToAdd = 
        [
          [(equalityVar, True), (v1, False), (v2, False)],
          [(equalityVar, True), (v1, True), (v2, True)],
          [(equalityVar, False), (v1, False), (v2, True)],
          [(equalityVar, False), (v1, True), (v2, False)]
        ]
    mapM_ addHardClause clausesToAdd 
    return equalityVar 


defineGT :: Variable -> Variable -> V.Vector Variable -> TWCalc Variable
defineGT larger smaller r = 
  do 
    gtVar <- NamingVariable <$> auxiliaryVariables 1 
    {- gt -> largerTrue smaller false restEqual-}
    mapM_ addHardClause .  map ((gtVar,False):) $ leftToRightClauses 
    {- largerTrue smallerFalse RestEqual -> gt-}
    addHardClause . ((gtVar,True):) $ rightToLeft
    return gtVar  
  where 
    rest = V.toList r
    leftToRightClauses = ([(smaller, False)]:) . ([(larger, True)]:) . map (\v -> [(v, True)]) $ rest
    rightToLeft = ((larger, False):) . ((smaller, True):) . map (flip (,) False) $ rest 
   
{---------------------------------------------------------------------------------------------------------------------}



{-
	Transitivity clauses in order to make sure that the order nodes really define a total order
-}
allTransitivityConstraints :: TWCalc ()
allTransitivityConstraints = asksFromInfo totNodes >>= 
  \x -> addAllClausesOfCertainType transitivityOfOrderVariables [(n1, n2, n3) | n1 <-[1..x], n2 <- [1..x], n3 <-[1..x] , n1 /= n2, n2 /= n3, n3 /= n1]


transitivityOfOrderVariables :: (Node, Node, Node) -> TWCalc ()
transitivityOfOrderVariables (n1, n2, n3) = 
  asksFromInfo widthOrderF >>= \f -> addHardClause [(f n1 n2, not $ widthOrderBool n1 n2), (f n2 n3, not $ widthOrderBool n2 n3), (f n1 n3, widthOrderBool n1 n3)]

{----------------------------------------ADD ARCS FOR EACH EDGE IN GRAPH----------------------------------------------}

{-
	Clauses that add an arc j -> i for every edge in the graph and j before i in the ordering.
	These are added for undirected pairs 
-}     

origArcEdgeClauses :: TWCalc ()
origArcEdgeClauses =  asksFromInfo edges >>= addAllClausesOfCertainType arcForAnEdge 

arcForAnEdge :: Edge -> TWCalc ()
arcForAnEdge (p1, p2) =
  do
	{- assumes p1 < p2 -}
    orderVar <- (\f -> f p1 p2) <$> asksFromInfo widthOrderF
    arcVarp1p2 <- (\f -> f p1 p2) <$> asksFromInfo arcInTriangulation 
    arcVarp2p1 <- (\f -> f p2 p1) <$> asksFromInfo arcInTriangulation 
    addHardClause [(orderVar, False), (arcVarp1p2, True)]
    addHardClause [(orderVar, True), (arcVarp2p1, True)]

{-
	REDUNDANT CLAUSES based on resolving the allArcEdgeClauses and propagateAllArcs clauses
-}
resolvedArcEdgeClauses :: TWCalc ()
resolvedArcEdgeClauses = asksFromInfo edges >>= addAllClausesOfCertainType redundantClausesEdge 

redundantClausesEdge :: Edge -> TWCalc ()
redundantClausesEdge (p1, p2) =
  do
	{- assumes p1 < p2 -}
    arcVarp1p2 <- asksFromInfo arcInTriangulation <*> return p1 <*> return p2  
    arcVarp2p1 <- asksFromInfo arcInTriangulation <*> return p2 <*> return p1  
    addHardClause [(arcVarp1p2, True), (arcVarp2p1, True)]

{---------------------------------------------------------------------------------------------------------------------}

{--------------------------------PROPAGATE ARCS FOR PREDECESSORS------------------------------}
{- 
	Propagate arcs based on other arcs (shared predecessors in the ordering)
-}

propagateAllArcs :: TWCalc ()
propagateAllArcs = asksFromInfo totNodes >>= 
  \n -> addAllClausesOfCertainType propagateArcs [(i,j,l) | i<-[1..n], j<-[1..n], l<-[1..n], i /= j, i /= l, j < l]

propagateArcs :: (Node, Node, Node) -> TWCalc ()
propagateArcs (p1, p2, p3) =
  do 
    {- Assume p1 /= p2, p1 /= p3 and p2 < p3. See paper on treewidth.  -} 
    orderVar <- asksFromInfo widthOrderF <*> (return p2) <*> (return p3)
    arcVarF <-  asksFromInfo arcInTriangulation 
    addHardClause [(arcVarF p1 p2, False), (arcVarF p1 p3, False), (orderVar, False), (arcVarF p2 p3, True)]
    addHardClause [(arcVarF p1 p2, False), (arcVarF p1 p3, False), (orderVar, True), (arcVarF p3 p2, True)]

------------------------------------------
resolvedPropagateArcClauses :: TWCalc ()
resolvedPropagateArcClauses = asksFromInfo totNodes >>= 
  \n -> addAllClausesOfCertainType redundantClausePropagatedArcs [(i,j,l) | i<-[1..n], j<-[1..n], l<-[1..n], i /= j, i /= l, j < l]

redundantClausePropagatedArcs :: (Node, Node, Node) -> TWCalc ()
redundantClausePropagatedArcs (p1,p2,p3) = 
  do 
    {- Assume p1 /= p2, p1 /= p3 and p2 < p3. See paper on treewidth.  -} 
    arcVarF <-  asksFromInfo arcInTriangulation 
    addHardClause [(arcVarF p1 p2, False), (arcVarF p1 p3, False), (arcVarF p3 p2, True), (arcVarF p2 p3, True)]

{---------------------------------------------------------------------------------------------------------------------}

{---------------------------------ENFORCE MINIALIZATION OF TW----------}
{-
	Next the clauses that enforce a conflict if the treewidth gets too big. 
-}
conflictOnBigTreeWidthAllNodes :: Int -> Int -> TWCalc ()
conflictOnBigTreeWidthAllNodes lb ub =
  do
    totalNodes <- asksFromInfo totNodes 
    variables <- mapM (conflictOnBigTreeWidthNode ub) . enumFromTo 1 $ totalNodes
    softClauseVars <- V.fromList . map NamingVariable . take ub . enumFrom <$> auxiliaryVariables ub
    let
       corectVars = map (\i -> map ((V.! i)) variables) [0..(ub - 1)]
    mapM_ makeEquivalences . zip (V.toList softClauseVars) . map (V.fromList) $ corectVars 
    makePropagation softClauseVars 
    let 
      (lbV, softV) = V.splitAt lb softClauseVars
    if (lb > 0) then V.mapM_ (\v -> addHardClause [(v, False)]) lbV >> 
                     V.mapM_ (\v -> addClause 1 [(v,True)]) softV  
                else V.mapM_ (\v -> addClause 1 [(v,True)]) softClauseVars


makeEquivalences :: (Variable, V.Vector Variable) -> TWCalc ()
makeEquivalences (sClaVar, cNetVars) = leftToRight cNetVars >> rightToLeft cNetVars
 where
  leftToRight = mapM_ addHardClause . map (\v -> [(sClaVar, False), (v, False)]) . V.toList
  rightToLeft = addHardClause . ((sClaVar, True):) . V.toList . V.map (flip (,) True)

makePropagation :: V.Vector Variable -> TWCalc ()
makePropagation = prop . V.toList
 where
  prop (x:[]) = return ()
  prop (x:y:ls) = addHardClause [(x, False), (y, True)] >> prop (y:ls)


conflictOnBigTreeWidthNode :: Int -> Node -> TWCalc (V.Vector Variable)
conflictOnBigTreeWidthNode ub p = 
  do 
    arcVarsF <- (\f -> f p) <$> asksFromInfo arcInTriangulation 
    asksFromInfo totNodes >>=  \n -> (cardNetworkAtLeastK ub . map arcVarsF . delete p . enumFromTo 1) n 



---------------------------------------------------------------------

{- NEEDED WHEN RESOLVING EITHER ARCS OR EDGES -}

{- Clauses that link the ordering to the arcs, the arcs only travel forward in the ordering -}
arcsOnlyTravelForvard :: TWCalc ()
arcsOnlyTravelForvard = asksFromInfo totNodes  >>= \n -> addAllClausesOfCertainType arcsForvard [(p1,p2) | p1<-[1..n], p2 <-[p1+1..n]]

{- Assumes p1 < p2 -}
arcsForvard :: (Node, Node) -> TWCalc ()
arcsForvard (p1, p2) =
  do
    orderVar <- (\f -> f p1 p2) <$> asksFromInfo widthOrderF  
    arcVarF <-  asksFromInfo arcInTriangulation
    addHardClause [(arcVarF p1 p2, False), (orderVar, widthOrderBool p1 p2)]
    addHardClause [(arcVarF p2 p1, False), (orderVar, widthOrderBool p2 p1)]

{- REDUNDANT break cycles in the arcs -}
breakCyclesInArcs :: TWCalc ()
breakCyclesInArcs = asksFromInfo totNodes  >>= \n -> addAllClausesOfCertainType breakArcCycle [(p1,p2) | p1<-[1..n], p2 <-[p1+1..n]]

breakArcCycle :: (Node, Node) -> TWCalc ()
breakArcCycle (p1,p2) = asksFromInfo arcInTriangulation >>= \f -> addHardClause [(f p1 p2, False), (f p2 p1, False)]

{- 
	A helper for getting the order variable in the correct form.
	As orders are only added for i < j we need to realize that j < i <--> NOT i < j 
        Hence we need to add a truth value too. As the variable function checks the order of p1 p2  we only need to check the truth value
-}

widthOrderBool :: Node -> Node -> Bool 
widthOrderBool p1 p2  
  | p1 < p2 = True
  | p1 > p2 = False
  | otherwise = undefined


{----------------------------------------- SAT HACK ---------------------------------------------------------------------------------}



conflictOnBigTreeWidthAllNodesSAT :: Int -> Int -> TWCalc ()
conflictOnBigTreeWidthAllNodesSAT _ ub = 
  enumFromTo 1 <$> asksFromInfo totNodes >>= 
  mapM_ (conflictOnBigTreeWidthNodeSAT ub)

conflictOnBigTreeWidthNodeSAT :: Int -> Node -> TWCalc ()
conflictOnBigTreeWidthNodeSAT ub p = 
  do 
    arcVarsF <- (\f -> f p) <$> asksFromInfo arcInTriangulation 
    asksFromInfo totNodes >>=  \n -> (improvedSequentialCounter (n-1) ub . map arcVarsF . delete p . enumFromTo 1) n 























