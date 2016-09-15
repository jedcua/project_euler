module Prob19 (module Prob19) where

-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4,
-- but not on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?

import Control.Monad.State

type Year  = Int
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Enum, Eq, Ord)
data Day   = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Show, Eq, Ord, Enum)

type Date = (Year, Month, Int)

numDays :: Month -> Year -> Int
numDays Sep _ = 30
numDays Apr _ = 30
numDays Jun _ = 30
numDays Nov _ = 30
numDays Feb y = if leapYear y then 29 else 28
numDays _   _ = 31

leapYear :: Int -> Bool
leapYear y = (divBy 4 && not (divBy 100)) || (divBy 100 && divBy 400) where
    divBy k  = y `mod` k == 0

nextDay :: Date -> Date
nextDay (year, month, day) = if succ day > numDays month year
                                 then if month == Dec
                                          then (succ year, Jan, 1)
                                          else (year, succ month, 1)
                                 else (year, month, succ day)

datesWithDay :: [(Day, Date)]
datesWithDay = zip days dates where
    days  = cycle [Sun .. Sat]
    dates = takeWhile (<= (2000, Dec, 31)) $ iterate nextDay (1900, Jan, 7)

firstSundays :: (Day, Date) -> State Int ()
firstSundays (Sun, (_,_,1)) = modify' (+1)
firstSundays _              = modify' id

answer :: Int
answer = execState (mapM firstSundays datesWithDay') 0 where
    datesWithDay' = filter (\(_,d) -> d >= (1901, Jan, 1)) datesWithDay
