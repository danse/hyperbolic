module Intervals where

import Hyperbola
import qualified Conf as C

type Interval = [Int]
type Multiplier = Float

-- Rated interval. Kept short for better accessors
data Rated = Rated {
  ratedRate :: Float,
  ratedInterval :: Interval
  } deriving Show

ratedCost r = (ratedRate r) * (fromIntegral $ length $ ratedInterval r)

combineRated a b = Rated averageRate sumOfIntervals
  where sumOfIntervals = (ratedInterval a) ++ (ratedInterval b)
        totalLength = fromIntegral $ length sumOfIntervals
        averageRate = ((ratedCost a) + (ratedCost b)) / totalLength

instance Monoid Rated where
  mempty = Rated 0 []
  mappend = combineRated

data Allocation = Allocation {
  allocationRate :: Float,
  allocationHours :: Int
  }

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
        adjustedRate = (ratedCost rated)/(fromIntegral C.target)

averageRate :: Multiplier -> Interval -> Float
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
  where t = quot (C.total - hours) 2
        f = t
        l = t + hours


{-

The code here is a bit hard to read, because an hour index also
corresponds to an amount of hours. The idea here is to pick cheap
hours for a new allocation as long as they don't conflict with the
personal development goal

The logic here is, more or less:

- reserve the most expensive hours for personal development
- allocate busy hours leaving the cheapest slots free
- allocate requested hours starting from the cheapest slots

-}
secondAllocation :: Float -> Int -> Int -> Float
secondAllocation rate busy requested
  | overflowing  = averageRate mul (cheapInterval ++ expensiveInterval)
  | otherwise    = averageRate mul cheapInterval
  where 
    cheapFree = C.target - busy
    overflowing = requested > cheapFree
    cheap = if overflowing then cheapFree else requested
    expensive = requested - cheap
    cheapestAllocated = C.personal + busy
    cheapInterval = [C.total-cheap+1..C.total]
    expensiveInterval = [C.personal-expensive+1..C.personal]
    getMultiplier = multiplierFromRated . adjustRateToTarget
    mul = getMultiplier (Rated rate [C.personal+1..cheapestAllocated])

totalCostTotalTime rate busy totalHours perWeek = (totalCost, totalTime)
  where totalCost = (secondAllocation rate busy perWeek) * totalHours
        totalTime = totalHours / (fromIntegral perWeek)
