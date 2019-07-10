module DIMACParser (parseADIMACFile) where

import Text.ParserCombinators.Parsec.Combinator 
import Control.Monad.Reader
import Text.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.Maybe (fromJust, catMaybes)
-- OWN
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as M
import Data.List (nub, sort, mapAccumL)

type GParser = Parsec String ()


parseADIMACFile :: String -> Either ParseError G.UGr 
parseADIMACFile =  runP parseWholeGraph  () ""

parseWholeGraph :: GParser G.UGr 
parseWholeGraph = 
  do
    edges <-  catMaybes <$> (many1 parseEdges <?> "EDGES")
    let 
      mapping = M.fromList $ zip (sort . nub . concatMap (\(p1, p2) -> [p1, p2]) $ edges) [1..]
      {- fromJust is okay as the map is constructed based on the lements of edges -}
      unsafeLookup = fromJust . (flip M.lookup) mapping
      convert mI (p1,p2) = (max mI hP, (lP, hP)) 
        where 
          rp1 =  unsafeLookup p1
          rp2 = unsafeLookup p2
          hP =  max rp1 rp2
          lP = min rp1 rp2
      (realNodes, realEdges) =  (\(mNode, edges) -> (mNode, nub edges)) . mapAccumL convert 0 $ edges
    return $ G.mkUGraph [1..realNodes] realEdges

parseEdges :: GParser (Maybe G.Edge)
parseEdges =
  do
    firstChar <- (anyChar <* spaces) <?> "First Char of EdgeLine" 
    if firstChar /= 'e' 
       then (((manyTill anyChar newLineOrEof) <* spaces) *> return Nothing) <?> "Skipping N line"
       else readRest <?> "ReadRest"
  where
    readRest = 
      do
        pointOne <- read <$> (manyTill anyChar space <* spaces) <?> "First point"
        pointTwo <- read <$> (manyTill anyChar spaceOrEof <* spacesOrEof) <?> "Second point"
        return $ Just (min pointOne pointTwo, max pointOne pointTwo)


newLine = char '\n'
newLineOrEof = (void $ try newLine) <|> eof


spaceOrEof =  (void $ try space) <|> (try eof)
spacesOrEof = (void $ try spaces) <|> (try eof)


