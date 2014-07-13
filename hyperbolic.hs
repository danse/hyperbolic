import Text.PrettyPrint.Boxes

average x = s/n
  where n = fromIntegral $ length x
        s = sum x

-- hyperbole a a == 1
-- hyperbole 1 a == a
-- hyperbole a 0 == Infinity
hyperbole :: Float -> Float -> Float
hyperbole k x = k/x

hoursPerWeek = 40

cheapest hour = 41 - hour

multiplier = 330

priceLine hour = (b employedHours) <+> (b hourPrice) <+> (b monthPrice)
  where employedHours = cheapest hour
        hourPrice = hyperbole multiplier hour
        monthPrice = 4 * hoursPerWeek * hourPrice
        b = text . show

priceTable = vcat left $ map priceLine [1..hoursPerWeek]

printPriceTable = printBox priceTable

toPrice :: Int -> Float
toPrice = (hyperbole multiplier) . cheapest . fromIntegral

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

-- main = do printPriceTable
