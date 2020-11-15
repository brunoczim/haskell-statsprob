module Data.ProbStat.Erf (erf, erfUntil, invErf, invErfUntil) where

import Data.ProbStat.Sum (sum, sumState)
import Prelude hiding (sum)
import Data.Vector (generate, (!))

erf :: Double -> Double
erf = erfUntil 1000

invErf :: Double -> Double
invErf = invErfUntil 1000

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
