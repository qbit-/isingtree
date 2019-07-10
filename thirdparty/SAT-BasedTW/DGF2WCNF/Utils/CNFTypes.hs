
module CNFTypes where

import PropLogic

import System.IO

import Control.Monad.Reader
import Control.Monad.State.Lazy

type ClausesAdded = Int
type HighestVariable = Int
type CNFState = (ClausesAdded, HighestVariable)

type ClauseAdder i a = ReaderT (i, Handle, Bool) (StateT CNFState IO) a


type ClauseAdderNoNewVar i = ClauseAdder i ()
type ClauseAdderAuxVar i = ClauseAdder i [Variable]

{- 
	When converting to CNF there is two things we always need to have in our problem info.
	The weight of the hard clauses and the highestr variable number taken (before calculations)
-}
class ProblemInfo i where
  hardClauseW :: i -> Weight
  highestV :: i -> Int
  ttP :: i -> Int


