module Lib
    ( erf
    , invErf
    , erfUntil
    , invErfUntil
    , stdNormal
    , invStdNormal
    ) where

import Prelude hiding (sum)
import Data.Foldable (foldl')
import Data.Vector (generate, (!))

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

stdNormal :: Double -> Double
stdNormal z = sqrt (pi / 2) / sqrt (2 * pi) * erf (z / sqrt 2)

invStdNormal :: Double -> Double
invStdNormal p = invErf (p * sqrt (2 * pi) / sqrt (pi / 2)) * sqrt 2

erf :: Double -> Double
erf = erfUntil 60

invErf :: Double -> Double
invErf = invErfUntil 100

erfUntil :: Int -> Double -> Double
erfUntil b z = 2 / sqrt pi * fst (sumState term 0 b 1)
  where
    term i state = (term', state')
      where
        state' = state * (-(z ** 2)) / (toEnum i + 1)
        term' = z / (2 * (toEnum i) + 1) * state

invErfUntil :: Int -> Double -> Double
invErfUntil b z = sum term 0 b
  where
    term i = coefs ! i / power * (sqrt pi / 2 * z) ** power
      where
        power = 2 * (toEnum i) + 1
    coefs = generate (b + 1) coef
    coef 0 = 1
    coef k = sum term 0 (k - 1) 
      where
        term i = numer / denom
          where
            numer = coefs ! i * coefs ! (k - 1 - i)
            denom = (toEnum i + 1) * (2 * toEnum i + 1)
