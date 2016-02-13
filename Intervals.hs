module Intervals where

import Hyperbola

type Interval = [Int]

{-

Let's say that i want to kwon the cumulative rate of hours between the
second and the sixt:

    r = k/2 + k/3 + k/4 + k/5 + k/6 = k (1/2 + 1/3 + 1/4 + 1/5 + 1/6)

This is related to the harmonic series, which is divergent. Here we
are interested in a small number of iterations though, corresponding
to the number of hours which fit in a working week

-}
harmonicSum :: Interval -> Float
harmonicSum []     = 0
harmonicSum (x:xs) = 1/(fromIntegral x) + (harmonicSum xs)

multiplierFromPayedInterval :: Float -> Interval -> Float
multiplierFromPayedInterval rate int = 
  let totalWeight = harmonicSum int
      totalPaid = rate * (fromIntegral (length int))
  in totalPaid/totalWeight

averageRate :: Float -> Interval -> Float
averageRate mul hours = 
  let hoursNumber = length hours
      totalPayment = sum $ map (hyperbola mul . fromIntegral) hours
  in totalPayment/(fromIntegral hoursNumber)

-- build an interval with the given amount of hours, allocating them
-- in the middle of the week in order to free both cheaper and more
-- expensive hours. Prefer allocating cheaper hours if uneven
--
-- >>> centeredInterval 34 == [1..34]
-- True
--
-- >>> centeredInterval 6 == [14..20]
-- True
centeredInterval :: Int -> Interval
centeredInterval hours = [f..l]
  where t = quot (hoursPerWeek - hours) 2
        f = t
        l = t + hours

hoursPerWeek = 35 :: Int
