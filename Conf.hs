module Conf where

daily = 7
perWeek = 5 * daily :: Int

total = perWeek -- * 4 for months instead of weeks

-- personal development: time for studying, free software, logistics,
-- connections etcetera. This is set rather low, for now, because the
-- algorithm in itself is aimed at protecting hours useful for
-- personal development
personal = round (0.1 * (fromIntegral total))
target = total - personal
