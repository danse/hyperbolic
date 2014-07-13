import Text.PrettyPrint.Boxes

average x = s/n
  where n = fromIntegral $ length x
        s = sum x

-- hyperbole a a == 1
-- hyperbole 1 a == a
-- hyperbole a 0 == Infinity
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

toPrice = (hyperbole multiplier) . cheapest

toPrices = map toPrice

priceForInterval = sum . toPrices

-- | Given an interval of hours, calculate the corresponding average hour price
hourlyPriceForInterval = average . toPrices

main = do printPriceTable
