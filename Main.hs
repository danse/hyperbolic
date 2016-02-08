import Text.PrettyPrint.Boxes hiding ((*))

import Hyperbola
import Intervals

average x = s/n
  where n = fromIntegral $ length x
        s = sum x

cheapest hour = hoursPerWeek + 1 - hour

priceLine :: Float -> Int -> Box
priceLine mul hour = employedHours <+> hourPriceBox <+> monthPrice
  where employedHours = text $ show $ cheapest hour
        hourPrice = hyperbola mul (fromIntegral hour)
        hourPriceBox = text $ show $ hourPrice
        monthPrice = text $ show $ 4 * (fromIntegral hoursPerWeek) * hourPrice

priceTable mul = vcat left $ map (priceLine mul) [1..hoursPerWeek]

printPriceTable mul = printBox $ priceTable mul

toPrice :: Float -> Int -> Float
toPrice multiplier = (hyperbola multiplier) . fromIntegral . cheapest

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
