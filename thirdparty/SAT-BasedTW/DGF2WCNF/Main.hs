module Main where

import DIMACParser
import CNFGeneration
import Control.Applicative 
import Data.Either.Utils
import TreeWidthClauses 
import VariableEnumeration
import Control.Applicative
import System.Directory 
import System.FilePath
import System.Exit
import System.Environment
import qualified Data.Graph.Inductive.Graph as G

instancesToTry = map (\(a,b,c,d) -> TWInstance a b c d) [(level, edges, arcs, breakArcCycles) | level <-[False], edges <-[True], arcs <-[True], breakArcCycles <-[True]]

main = 
  do
    args <- getArgs
    if length args /= 1
      then putStrLn "Need 1 argument, name of .dgf file to turn to .wcnf file" >> exitFailure
      else return ()
    processGraph (head args)
   
    

processGraph :: FilePath -> IO([FilePath]) 
processGraph f = 
  do
    putStrLn  ("Processing " ++ f) 
    g <-  forceEitherMsg "Graph parsing failed" . parseADIMACFile <$> (readFile f)
    let e  = createVariableEnumerations g
        lb =  0
        ub =  subtract 1 . totNodes $ e  
    if totNodes e > 200
     then putStrLn "The graph is too big" >> return []
     else mapM (processOneSpec lb ub e) instancesToTry 
  where
    finalName lb ub s =  flip addExtension "wcnf" . dropExtension $ f 
    processOneSpec lb ub e s = runClauseAdderRecordResultsMaxSAT (finalName lb ub s) e (allTreewidthClauses lb ub s)

