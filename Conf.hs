module Conf where

daily = 7
perWeek = 5 * daily :: Int

total = perWeek -- * 4 for months instead of weeks
-- personal development: time for studying etcetera
personal = round (0.3 * (fromIntegral total))
target = total - personal
