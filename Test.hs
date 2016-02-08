import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Control.Monad
import System.Exit

import Hyperbola

-- for all `a` greater than zero, because 0/0 == NaN. There has to be
-- a more elegant way to express this
sameNumberGivesOne a = (a <= 0) || ((hyperbola a a) == 1) :: Bool
coefficientOneGivesIdentity a = (hyperbola a 1) == a :: Bool
alwaysInfinityAtZero a = (a <= 0) || ((hyperbola a 0) == 1/0) :: Bool

main :: IO ()
main = do
  result <- quickCheckResult sameNumberGivesOne
  unless (isSuccess result) exitFailure
  result <- quickCheckResult coefficientOneGivesIdentity
  unless (isSuccess result) exitFailure
  result <- quickCheckResult alwaysInfinityAtZero
  unless (isSuccess result) exitFailure
