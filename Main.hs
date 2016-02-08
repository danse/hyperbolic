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
harmonicSum first last
  | first > last = 0
  | otherwise    = 1/first + others
  where others = harmonicSum (first+1) last

hoursPerWeek = 35

cheapest hour = 41 - hour

priceLine mul hour = (b employedHours) <+> (b hourPrice) <+> (b monthPrice)
  where employedHours = cheapest hour
        hourPrice = hyperbola mul hour
        monthPrice = 4 * hoursPerWeek * hourPrice
        b = text . show

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
