module Intervals where

import Hyperbola

data Interval = Interval { firstHour :: Int, lastHour :: Int } deriving Show

{-

Let's say that i want to kwon the cumulative rate of hours between the
second and the sixt:

    r = k/2 + k/3 + k/4 + k/5 + k/6 = k (1/2 + 1/3 + 1/4 + 1/5 + 1/6)

This is related to the harmonic series, which is divergent. Here we
are interested in a small number of iterations though, corresponding
to the number of hours which fit in a working week

-}
harmonicSum :: Interval -> Float
harmonicSum (Interval { firstHour=f, lastHour=l })
  | f > l  = 0
  | otherwise = (1/(fromIntegral f)) + others
  where others = harmonicSum (Interval { firstHour=f+1, lastHour=l })

multiplierFromPayedInterval :: Float -> Interval -> Float
multiplierFromPayedInterval rate int = 
  let totalWeight = harmonicSum int
      totalPaid = rate * (fromIntegral ((lastHour int) - (firstHour int) + 1))
  in totalPaid/totalWeight

averageRate :: Float -> Interval -> Float
averageRate mul (Interval { firstHour=firstHour, lastHour=lastHour }) = 
  averageRateDiscontinuous mul [firstHour..lastHour]

averageRateDiscontinuous :: Float -> [Int] -> Float
averageRateDiscontinuous mul hours = 
  let hoursNumber = length hours
      totalPayment = sum $ map (hyperbola mul . fromIntegral) hours
  in totalPayment/(fromIntegral hoursNumber)

-- build an interval with the given amount of hours, allocating them
-- in the middle of the week in order to free both cheaper and more
-- expensive hours. Prefer allocating cheaper hours if uneven
--
-- >>> centeredInterval 34
-- Interval { firstHour: 1, lastHour: 34 }
--
-- >>> centeredInterval 6
-- Interval { firstHour: 14, lastHour: 20 }
centeredInterval :: Int -> Interval
centeredInterval hours = Interval { firstHour=f, lastHour=l }
  where t = quot (hoursPerWeek - hours) 2
        f = t
        l = t + hours

hoursPerWeek = 35 :: Int
