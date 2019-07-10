module CNFGeneration 
  (runClauseAdderRecordResultsMaxSAT,
   runClauseAdderRecordResultsSAT, 
   addAllClausesOfCertainType, 
   addClause,
   addHardClause,
   auxiliaryVariables,
   asksFromInfo,
   testCLauseAdder) where

import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Control.Monad.Reader
import Control.Monad.State.Lazy


import PropLogic
import CNFTypes
import System.IO
import qualified Data.ByteString.Char8 as B
 
import Control.Applicative
import System.FilePath.Posix
import System.Cmd

{-
	A General Framework for handling clauses. 
	The monad is built upon a State, Writer and Reader Monad transformer.
	The idea is that the state keeps track of the current highest variable.
	The writer adds the clauses.
	The reader is used for info on the actual problem.  
	The one thing we always want to know is the weight of hard clauses. 
	The calculation of the monads can be used to define auxiliary variables 
	For computation efficiency we require the monad to be strict in the state.
-}
newtype Test = Test Int

instance ProblemInfo Test where
  hardClauseW (Test c) = fromIntegral c 
  highestV  (Test c) = c 
  ttP (Test c) = c 

testCLauseAdder :: Int ->  ClauseAdderNoNewVar Test -> IO ()
testCLauseAdder i c = runClauseAdderT False stdout (Test (i)) c >> return ()

runClauseAdderRecordResultsMaxSAT  :: ProblemInfo i => FilePath -> i -> (ClauseAdderNoNewVar i) -> IO(FilePath)
runClauseAdderRecordResultsMaxSAT = runClauseAdderRecordResults False

runClauseAdderRecordResultsSAT :: ProblemInfo i => FilePath -> i -> (ClauseAdderNoNewVar i) -> IO(FilePath)
runClauseAdderRecordResultsSAT = runClauseAdderRecordResults True


runClauseAdderRecordResults :: ProblemInfo i => Bool -> FilePath -> i -> (ClauseAdderNoNewVar i) -> IO(FilePath)
runClauseAdderRecordResults makeSAT solFile info clausesToUse = 
  do
    cnf <- openFile tmpFile WriteMode
    (amCla, var) <- runClauseAdderT makeSAT cnf info clausesToUse
    hClose cnf
    let      
      headerLine = unwords $ ["p", if makeSAT then "cnf" else "wcnf", show var, show amCla, if makeSAT then "" else show . round . hardClauseW $ info, "\n"]
    writeFile temporaryF headerLine >> system catCommand >> system removeTempFile >> void (system removeFileToAdd) 
    return solFile
 where
  tmpFile = (replaceExtension solFile "tmp")
  temporaryF = (tmpFile ++ "temp")
  catCommand = unwords ["cat", temporaryF, tmpFile,  ">", solFile]
  removeTempFile = unwords ["rm", temporaryF]
  removeFileToAdd = unwords ["rm", tmpFile]



{-
	The function to run to execute the calculation of the clauses and the 
	Highest variable + clauses used (header line) 
	Best performance achieved when the state calculations are done without refering 
	to the clauses. 
-}


runClauseAdderT :: ProblemInfo i => Bool -> Handle -> i -> (ClauseAdderNoNewVar i) -> IO CNFState
runClauseAdderT makeSAT cnf info clauses = stateC
 where
  readerC = runReaderT clauses (info, cnf, makeSAT)
  stateC = execStateT readerC (0, highestV info)

{-
	Generic helper that takes a clause adder for a single "unit" and maps it over a suplied List.
	Often done because we wish to add a certain type of constraint over a lot of things
-}
addAllClausesOfCertainType :: (x -> ClauseAdderNoNewVar i) -> [x] -> ClauseAdderNoNewVar i
addAllClausesOfCertainType f ls = sequence_  $ map f ls

{- 
	When auxiliary variables are needed, this function should be called in order to 
	Guarantee consistent adding of variables. 
-}

auxiliaryVariables :: ProblemInfo i => Int -> ClauseAdder i  Int
auxiliaryVariables newVars =
  do 
    (cl, ind) <- get
    put $! (cl, ind+newVars)
    return . (+1) $ ind

{-
  Helper to add a clause and correctly update the count. This takes a weight.
-}
addClause :: Weight -> Clause -> ClauseAdderNoNewVar i
addClause w cl = 
  do 
    (handle, cnf) <- asks (\(_, y, z) -> (y,z))
    liftIO $ B.hPutStrLn handle . B.pack . weightedClauseToString cnf $ (cl, w)
    (cl, v) <- get
    put $! (cl + 1, v)

{-
  To add a hard clause we just retrieve the max weight and then add a clause with that weight
-}
addHardClause :: ProblemInfo i => [Literal] -> ClauseAdderNoNewVar i
addHardClause cl = asksFromInfo hardClauseW >>= flip addClause cl
    
asksFromInfo ::  ProblemInfo i => (i -> a) -> ClauseAdder i a
asksFromInfo f = asks (f . (\(x, _, _) -> x))



