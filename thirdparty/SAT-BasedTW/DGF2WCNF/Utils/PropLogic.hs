module PropLogic where


-- Used to print  the weights "smartly"
import Text.Printf

type Weight = Double
type Literal = (Variable, Bool)
type Clause = [Literal]
type WClause = (Clause, Weight) 

newtype Variable = NamingVariable {
                                 variableN :: Int
                               } deriving (Eq, Ord)

instance Show Variable where
  show = show . variableNumber

variableNumber :: Variable -> Int 
variableNumber = variableN
                           
clauseToString :: Clause -> String
clauseToString = (++" 0") . unwords . map (\(w, b) -> concat [sign b, show w])
 where
  sign b = if b then "" else "-" 

weightedClauseToString :: Bool ->  WClause -> String
weightedClauseToString b (cl, w) = unwords [if b then "" else printW w, clauseToString cl]


printW = printf "%.0f" 




