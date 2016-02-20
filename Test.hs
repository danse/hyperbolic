import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Control.Monad
import System.Exit

import Hyperbola
import Intervals

-- for all `a` greater than zero, because 0/0 == NaN. There has to be
-- a more elegant way to express this
sameNumberGivesOne a = (a <= 0) || ((hyperbola a a) == 1) :: Bool
coefficientOneGivesIdentity a = (hyperbola a 1) == a :: Bool
alwaysInfinityAtZero a = (a <= 0) || ((hyperbola a 0) == 1/0) :: Bool

intervalsInverse a b c =
  let int = [b..c]
      skip = (a <= 0) || (b <= 0) || (c <= 0) || (c < b)
      diff = averageRate (multiplierFromInterval a int) int - a
      prop = (abs diff) < 0.001
  in skip || prop

main :: IO ()
main = do
  result <- quickCheckResult sameNumberGivesOne
  unless (isSuccess result) exitFailure
  result <- quickCheckResult coefficientOneGivesIdentity
  unless (isSuccess result) exitFailure
  result <- quickCheckResult alwaysInfinityAtZero
  unless (isSuccess result) exitFailure
  result <- quickCheckResult intervalsInverse
  unless (isSuccess result) exitFailure
