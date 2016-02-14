module Intervals where

import Hyperbola

type Interval = [Int]
-- Rated interval. Kept short for better accessors
data Rated = Rated {
  ratedRate :: Float,
  ratedInterval :: Interval
  } deriving Show

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

multiplierFromInterval :: Float -> Interval -> Float
multiplierFromInterval rate interval = 
  let totalWeight = harmonicSum interval
      totalPaid = rate * (fromIntegral (length interval))
  in totalPaid/totalWeight

multiplierFromRated :: [Rated] -> Float
multiplierFromRated rated =
  let i = foldl (++) [] (map ratedInterval rated)
      cumulate r = (ratedRate r)*(fromIntegral $ length $ ratedInterval r)
      r = (sum (map cumulate rated))/(fromIntegral hoursPerWeek)
    in multiplierFromInterval r i

averageRate :: Float -> Interval -> Float
averageRate mul interval = 
  let hoursNumber = length interval
      totalPayment = sum $ map (hyperbola mul . fromIntegral) interval
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

secondAllocation :: Float -> Int -> Int -> Float
secondAllocation rate busyHours hoursRequested =
  let firstFreeHour = hoursPerWeek-busyHours
      mul = multiplierFromRated [Rated rate [firstFreeHour+1..hoursPerWeek]]
  in averageRate mul [firstFreeHour-hoursRequested+1..firstFreeHour]
