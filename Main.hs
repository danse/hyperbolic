import Text.PrettyPrint.Boxes hiding ((*))

import Hyperbola

data Interval = Interval { firstHour :: Int, lastHour :: Int } deriving Show

average x = s/n
  where n = fromIntegral $ length x
        s = sum x

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
multiplierFromPayedInterval rate int = totalPaid/totalWeight
  where totalWeight = harmonicSum int
        totalPaid = rate * ((fromIntegral $ firstHour int) - (fromIntegral $ lastHour int))

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

cheapest hour = 41 - hour

priceLine :: Float -> Int -> Box
priceLine mul hour = employedHours <+> hourPriceBox <+> monthPrice
  where employedHours = text $ show $ cheapest hour
        hourPrice = hyperbola mul (fromIntegral hour)
        hourPriceBox = text $ show $ hourPrice
        monthPrice = text $ show $ 4 * (fromIntegral hoursPerWeek) * hourPrice

priceTable mul = vcat left $ map (priceLine mul) [1..hoursPerWeek]

printPriceTable mul = printBox $ priceTable mul

toPrice :: Float -> Int -> Float
toPrice multiplier = (hyperbola multiplier) . cheapest . fromIntegral

toPrices m = map (toPrice m)

putAllStrLn :: [String] -> IO [()]
putAllStrLn x = sequence $ map putStrLn x

pricesForInterval :: Float -> [Int] -> [String]
pricesForInterval m x = [weekly, hourly, monthly]
  where prices = toPrices m x
        weeklyPrice = sum prices
        hourlyPrice = average prices
        monthlyPrice = weeklyPrice * 4
        pre s v = s ++ ": " ++ (show v) -- present
        weekly = pre "weekly" weeklyPrice
        hourly = pre "hourly" hourlyPrice
        monthly = pre "monthly" monthlyPrice

intervalInfo mul = putAllStrLn . (pricesForInterval mul)

-- | Given an interval of hours, calculate the corresponding average hour price
hourlyPriceForInterval mul = average . (toPrices mul)

main = return ()
