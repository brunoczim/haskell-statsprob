module Data.ProbStat.Normal (stdNormal, invStdNormal) where

import Data.ProbStat.Erf (erf, invErf)

stdNormal :: Double -> Double
stdNormal z = sqrt (pi / 2) / sqrt (2 * pi) * erf (z / sqrt 2)

invStdNormal :: Double -> Double
invStdNormal p = invErf (p * sqrt (2 * pi) / sqrt (pi / 2)) * sqrt 2
