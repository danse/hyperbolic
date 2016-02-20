module Intervals where

import Hyperbola

type Interval = [Int]
-- Rated interval. Kept short for better accessors
data Rated = Rated {
  ratedRate :: Float,
  ratedInterval :: Interval
  } deriving Show

ratedCost r = (ratedRate r) * (fromIntegral $ length $ ratedInterval r)

combineRated a b = Rated averageRate sumOfIntervals
  where sumOfIntervals = (ratedInterval a) ++ (ratedInterval b)
        averageRate = ((ratedCost a) + (ratedCost b))/(fromIntegral $ length sumOfIntervals)

instance Monoid Rated where
  mempty = Rated 0 []
  mappend = combineRated

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

multiplierFromRated r = multiplierFromInterval (ratedRate r) (ratedInterval r)

adjustRateToTarget rated = Rated adjustedRate interval
  where interval = ratedInterval rated
        adjustedRate = (ratedCost rated)/(fromIntegral targetHours)

multiplierFromRateds :: [Rated] -> Float
multiplierFromRateds rated =
  let concatenated = mconcat rated
      adjusted = adjustRateToTarget concatenated
  in multiplierFromRated adjusted

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

dailyHours = 7
hoursPerWeek = 5 * dailyHours :: Int
targetHours = 4 * dailyHours -- the rest for studying etcetera

secondAllocation :: Float -> Int -> Int -> Float
secondAllocation rate busyHours hoursRequested =
  let firstFreeHour = hoursPerWeek-busyHours
      mul = multiplierFromRateds [Rated rate [firstFreeHour+1..hoursPerWeek]]
  in averageRate mul [firstFreeHour-hoursRequested+1..firstFreeHour]
