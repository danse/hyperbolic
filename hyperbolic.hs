import Text.PrettyPrint.Boxes

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

priceForInterval x = sum $ map (h . cheapest) x
  where h = hyperbole multiplier

-- | Given an interval of hours, calculate the corresponding average hour price
hourlyPriceForInterval x = s/n
  where n = fromIntegral $ length x
        s = priceForInterval x
