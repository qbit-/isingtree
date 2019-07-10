module CardinalityConstraints (cardNetworkAtLeastK, improvedSequentialCounter) where
-- sequentialCounterImp, sequentialCounterExactlyOne, ladderEncodingExactlyOne, ExactlyOneConstraint, atLeastOneEx, cardNetworkAtLeastK

import Control.Monad.Reader
import qualified Data.Vector as V
import Control.Applicative
--Own---
import PropLogic
import CNFTypes
import CNFGeneration
import Debug.Trace
-----


{-
	A module that implements cardinality constraints over variables. The constraints are defined in terms of the clause adder monad.
	Note that for consistency, the cardinality constraints that add variables, return the set of added variables, these can be discarded if need be.
 	To allow performance, we require the amount of vars to be added over to be given as input. Best performance is achieved when this number is not 
	counted from the variables (by calling length for example)
-}
improvedSequentialCounter :: ProblemInfo i => Int -> Int -> [Variable] -> ClauseAdder i ()
improvedSequentialCounter  amVars k v = 
  do 
    auxVars <- V.mapM (id) $ V.generate (amVars - 1) generateVars
    mapM_ (propagationClauses . V.toList . extractColumn auxVars) $ [0..(k-1)]
    V.zipWithM_ (\var ctr -> addHardClause [(var, False), (ctr, True)]) vars (V.map (V.head) auxVars) 
    mapM_ (\i -> countingClauses (vars V.! i) (V.toList . (auxVars V.!) . subtract 1 $ i) (V.toList . V.tail . (auxVars V.!) $ i) ) [1..amVars-2]
    mapM_ (\i-> addHardClause [ (vars V.! i, False),  (getKthVariable auxVars i , False)] ) [k..amVars-1]
 where 
  vars = V.fromList v
  amountExtraVars = min k . (+1) 
  generateVars i = (V.fromList . map (NamingVariable) . take (amountExtraVars i) . enumFrom)  <$> (auxiliaryVariables . amountExtraVars $ i)
  extractColumn vec i = V.map (V.! i) . V.drop i $ vec 
  getKthVariable vec i = (vec V.! (i-1)) V.! (k-1)


propagationClauses :: ProblemInfo i => [Variable] -> ClauseAdder i ()
propagationClauses [] = return ()
propagationClauses (x:[]) = return ()
propagationClauses (x:y:ls) = addHardClause [(x, False), (y,True)] >> propagationClauses (y:ls)

countingClauses :: ProblemInfo i => Variable -> [Variable] -> [Variable] -> ClauseAdder i ()
countingClauses arcVariable _ [] = return ()
countingClauses arcVariable (y:ls) (x:xs) = addHardClause [(arcVariable , False), (y, False), (x, True)] >> countingClauses arcVariable ls xs
countingClauses _ _ _ = undefined
 
{- 
  Cardinality based sorting networks top level wrapper that turns the list of Variables int oa 	vector in order to facilitate random access 
-}

cardNetworkAtLeastK :: ProblemInfo i => Int -> [Variable] -> ClauseAdder i (V.Vector Variable)
cardNetworkAtLeastK amVars vars = cardNetW (amVars+1) . V.fromList $ vars


{- Next methods follow the paper "Parametric approach for smaller and better encodings of cardinality constraints CP 2013 -}

{- Return a m-cardnetwork of the input variable -}
cardNetW :: ProblemInfo i => Int -> V.Vector Variable -> ClauseAdder i (V.Vector Variable)
cardNetW m vars 
  {- if we're already down to less variables that we want in the card network then we just sort -}
  | m >= V.length vars = sortN vars
  {- Otherwise we recursively create the network and merge the results. For now pick the pivot from the middle always. -}
  | otherwise = 
    do 
      let 
        l = (V.length vars) `div` 2
        (vec1, vec2) = V.splitAt l vars
      zVector <-  cardNetW m vec1
      zPrimeVec <- cardNetW m vec2
      sMerge m zVector zPrimeVec
    
{- Next a sorting network. Given input variables it produces output variables that are sorted. -}
sortN :: ProblemInfo i => V.Vector Variable -> ClauseAdder i (V.Vector Variable)
sortN vars  
  {- Input of length 1 is already sorted -}
  | amVars == 1 = return vars
  {- Input of length 2 can be merged -}
  | amVars == 2 =
    do 
      x1 <- V.headM vars 
      x2 <- V.lastM vars
      twoComp x1 x2
  | otherwise =
    do   
      let 
        l = (V.length vars) `div` 2
        (vec1, vec2) = V.splitAt l vars
      zVector <-sortN vec1
      zPrimeVec <- sortN vec2
      mergeN zVector zPrimeVec  
 where
  amVars = V.length vars


{- Given two sorted sequences merge them -}
mergeN :: ProblemInfo i => V.Vector Variable -> V.Vector Variable -> ClauseAdder i (V.Vector Variable) 
mergeN vec1 vec2  
  {- Base cases -}
  | a == 1 && b == 1 = 
    do 
      x1 <- V.headM vec1 
      x2 <- V.headM vec2 
      twoComp x1 x2
  | a == 0 = return vec2
  | b == 0 = return vec1 
  {- Recursion -}
  | isEven a && isEven b =
    do 
      oddZ <- mergeN (extractOdds vec1) (extractOdds vec2)
      evenZ <- mergeN (extractEvens vec1) (extractEvens vec2) 
      z1 <- V.headM oddZ
      zab <- V.lastM evenZ
      let
        restOdds = safeTail "A even, B even" oddZ
        restEvens = V.init evenZ
      yVector <- liftM (V.foldl (V.++) V.empty) $ V.zipWithM twoComp restEvens restOdds 
      return $ (z1 `V.cons` yVector) `V.snoc` zab
  | isEven a && isOdd b =
    do 
      oddZ <- mergeN (extractOdds vec1) (extractOdds vec2)
      evenZ <- mergeN (extractEvens vec1) (extractEvens vec2) 
      z1 <- V.headM oddZ
      let
        restOdds = safeTail "A even, B odd" oddZ
      yVector <- liftM (V.foldl1 (V.++)) $ V.zipWithM twoComp evenZ restOdds 
      return $ z1 `V.cons` yVector
  | isOdd a && isEven b = mergeN vec2 vec1
  | isOdd a && isOdd b = 
    do
      oddZ <- mergeN (extractOdds vec1) (extractOdds vec2)
      evenZ <- mergeN (extractEvens vec1) (extractEvens vec2) 
      z1 <- V.headM oddZ
      zab <- V.lastM oddZ
      let
        restOfOds =  V.slice 1 ((subtract 2) . V.length $ oddZ) $ oddZ
      y1Vector <- liftM (V.foldl (V.++) V.empty) $ V.zipWithM twoComp restOfOds evenZ
      return . (z1 `V.cons`) . (y1Vector `V.snoc`) $ zab
 where 
  a = V.length vec1
  b = V.length vec2 
  isEven x = (x `mod` 2) == 0
  isOdd = not . isEven
  indexIsEven i _ = isEven (i + 1)
  indexIsOdd i x = not $ indexIsEven i x 
  extractEvens = V.ifilter indexIsEven
  extractOdds = V.ifilter indexIsOdd   

safeTail :: String -> V.Vector Variable -> V.Vector Variable
safeTail s v = if V.length v == 0 then trace s $ V.tail v else V.tail v

{- Simple merge when we're only intereseted in initial bits -}

sMerge :: ProblemInfo i => Int -> V.Vector Variable -> V.Vector Variable -> ClauseAdder i (V.Vector Variable) 
sMerge c vec1 vec2
  | a == 1 && b == 1 && c == 1 = 
    do 
      x1 <- V.headM vec1 
      x2 <- V.headM vec2 
      firstNew <- auxiliaryVariables 1
      let 
       yVar = NamingVariable firstNew
      addHardClause [(x1, False), (yVar, True)]
      addHardClause [(x2, False), (yVar, True)]
      return . V.singleton $ yVar      
  | a > c = sMerge c (V.take c vec1) vec2
  | b > c = sMerge c vec1 (V.take c vec2)
  | (a + b) <= c = mergeN vec1 vec2 
  | isEven c = 
    do
      oddZ <- sMerge (c `div` 2 + 1) (extractOdds vec1) (extractOdds vec2)
      evenZ <- sMerge (c `div` 2) (extractEvens vec1) (extractEvens vec2) 
      z1 <- V.headM oddZ
      let
        restOdds = V.init . V.tail $ oddZ
        restEvens = V.init evenZ
      yVector <- liftM (V.foldl (V.++) V.empty) $ V.zipWithM twoComp restEvens restOdds 
      lastY <- liftM (NamingVariable) $ auxiliaryVariables 1
      addHardClause [(V.last oddZ, False), (lastY, True)]
      addHardClause [(V.last evenZ, False), (lastY, True)]
      return $ (z1 `V.cons` yVector) `V.snoc` lastY
  | isOdd c =
    do
      oddZ <- sMerge ((c + 1 ) `div` 2) (extractOdds vec1) (extractOdds vec2)
      evenZ <- sMerge ((c - 1) `div` 2) (extractEvens vec1) (extractEvens vec2) 
      z1 <- V.headM oddZ
      let
        restOdds =  V.tail oddZ
      yVector <- liftM (V.foldl (V.++) V.empty) $ V.zipWithM twoComp evenZ restOdds 
      return $ z1 `V.cons` yVector 
 where 
  a = V.length vec1
  b = V.length vec2 
  isEven x = (x `mod` 2) == 0
  isOdd x = (x `mod` 2) == 1 
  indexIsEven i _ = isEven (i + 1)
  indexIsOdd i _ = isOdd (i + 1)
  extractEvens = V.ifilter indexIsEven
  extractOdds = V.ifilter indexIsOdd  

twoComp :: ProblemInfo i => Variable -> Variable -> ClauseAdder  i (V.Vector Variable) 
twoComp x1 x2 =
  do
    ind <- auxiliaryVariables 2
    let
      y1 = NamingVariable ind
      y2 = NamingVariable (ind +1)
    addHardClause [(x1, False), (y1, True)]
    addHardClause [(x2, False), (y1, True)]
    addHardClause [(x1, False), (x2, False), (y2, True)]
{-
    addHardClause [(x1, True), (y2, False)]
    addHardClause [(x2, True), (y2, False)]
    addHardClause [(x1, True), (x2, True), (y1, False)]
-}
    return $ V.fromList [y1, y2]

