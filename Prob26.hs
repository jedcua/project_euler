module Prob26 where

import Data.Scientific
import Data.Ratio
import Data.List (sortBy)
import Data.Either.Unwrap (fromRight)
import Data.Maybe (isJust, fromJust)

-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions 
-- with denominators 2 to 10 are given:

-- 1/2	= 	0.5
-- 1/3	= 	0.(3)
-- 1/4	= 	0.25
-- 1/5	= 	0.2
-- 1/6	= 	0.1(6)
-- 1/7	= 	0.(142857)
-- 1/8	= 	0.125
-- 1/9	= 	0.(1)
-- 1/10	= 	0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has 
-- a 6-digit recurring cycle.

-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its 
-- decimal fraction part.

(/..?) :: Integer -> Integer -> Maybe [Int]
n /..? d = case recurring of
               Just recPos -> Just $ drop (recPos + decPtPos) digits
               Nothing     -> Nothing
           where (decimal, recurring) = fromRight $ fromRationalRepetend Nothing (n % d)
                 (digits , decPtPos)  = toDecimalDigits decimal

recurringPairs :: [(Integer, Maybe [Int])]
recurringPairs = sortBy recurringLen $ filter (isJust . snd) pairs
        where pairs = zip range recurring
              range = [1..1000]
              recurring = map (1 /..?) range

recurringLen :: (Integer, Maybe [Int]) -> (Integer, Maybe [Int]) -> Ordering
recurringLen (_, a) (_, b) = len' a `compare` len' b where
    len' = length . fromJust

answer :: String
answer = "Number " ++ show num ++ " Recurring digits: " ++ (show . length . fromJust) rs
        where (num, rs) = last recurringPairs
