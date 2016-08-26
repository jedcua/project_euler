-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?

import Data.Char

number = 2 ^ 1000

getDigits :: String -> [Int]
getDigits = map digitToInt

answer = sum digits where
	digits = getDigits (show number)
