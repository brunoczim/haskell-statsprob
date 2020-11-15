module Data.ProbStat.Sum (sum, sumState) where

import Prelude hiding (sum)
import Data.Foldable (foldl')

sum :: (Int -> Double) -> Int -> Int -> Double
sum term low high = foldl' step 0 [low .. high]
  where
    step acc i = term i + acc

sumState :: (Int -> a -> (Double, a)) -> Int -> Int -> a -> (Double, a)
sumState term low high init = foldl' step (0, init) [low .. high]
  where
    step (acc, state) i = (acc + val, state')
      where
        (val, state') = term i state
