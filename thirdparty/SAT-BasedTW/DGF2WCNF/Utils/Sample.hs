{-# LANGUAGE BangPatterns #-}
module Sample (sampleV, sampleVfixedSeed) where

import Control.Monad.Primitive
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import System.Random.MWC
import qualified Data.Vector as V


sampleV :: [a] -> Int -> IO [a]
sampleV ls s = create >>= sample ls s

sampleVfixedSeed :: Int -> [a] -> Int -> IO [a]
sampleVfixedSeed seed ls s = initialize (V.singleton (fromIntegral seed)) >>= sample ls s

sample :: PrimMonad m => [a] -> Int -> Gen (PrimState m) -> m [a]
sample ys size = go 0 (l - 1) (Seq.fromList ys) where
    l = length ys
    go !n !i xs g | n >= size = return $! (toList . Seq.drop (l - size)) xs
                  | otherwise = do
                      j <- uniformR (0, i) g
                      let toI  = xs `Seq.index` j
                          toJ  = xs `Seq.index` i
                          next = (Seq.update i toI . Seq.update j toJ) xs
                      go (n + 1) (i - 1) next g
{-# INLINE sample #-}
