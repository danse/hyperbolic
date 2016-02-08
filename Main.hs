import Text.PrettyPrint.Boxes

import Hyperbola

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
coefficientMultiplier first last
  | first > last = 0
  | otherwise    = 1/first + others
  where others = coefficientMultiplier (first+1) last

hoursPerWeek = 40

cheapest hour = 41 - hour

multiplier = 330

priceLine hour = (b employedHours) <+> (b hourPrice) <+> (b monthPrice)
  where employedHours = cheapest hour
        hourPrice = hyperbola multiplier hour
        monthPrice = 4 * hoursPerWeek * hourPrice
        b = text . show

priceTable = vcat left $ map priceLine [1..hoursPerWeek]

printPriceTable = printBox priceTable

toPrice :: Int -> Float
toPrice = (hyperbola multiplier) . cheapest . fromIntegral

toPrices = map toPrice

putAllStrLn :: [String] -> IO [()]
putAllStrLn x = sequence $ map putStrLn x

pricesForInterval :: [Int] -> [String]
pricesForInterval x = [weekly, hourly, monthly]
  where prices = toPrices x
        weeklyPrice = sum prices
        hourlyPrice = average prices
        monthlyPrice = weeklyPrice * 4
        pre s v = s ++ ": " ++ (show v) -- present
        weekly = pre "weekly" weeklyPrice
        hourly = pre "hourly" hourlyPrice
        monthly = pre "monthly" monthlyPrice

intervalInfo = putAllStrLn . pricesForInterval

-- | Given an interval of hours, calculate the corresponding average hour price
hourlyPriceForInterval = average . toPrices

main = printPriceTable
