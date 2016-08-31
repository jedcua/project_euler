-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 
-- 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
-- how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
-- contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use 
-- of "and" when writing out numbers is in compliance with British usage.

import Data.Char

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

parseTens :: Int -> Int -> String
parseTens 0 u = digitToWord u
parseTens 1 0 = "ten"
parseTens 1 1 = "eleven"
parseTens 1 2 = "twelve"
parseTens 1 3 = "thirteen"
parseTens 1 4 = "fourteen"
parseTens 1 5 = "fifteen"
parseTens 1 6 = "sixteen"
parseTens 1 7 = "seventeen"
parseTens 1 8 = "eighteen"
parseTens 1 9 = "nineteen"
parseTens 2 0 = "twenty"
parseTens 2 u = "twenty " ++ digitToWord u
parseTens 3 0 = "thirty"
parseTens 3 u = "thirty " ++ digitToWord u
parseTens 4 0 = "forty"
parseTens 4 u = "forty " ++ digitToWord u
parseTens 5 0 = "fifty"
parseTens 5 u = "fifty " ++ digitToWord u
parseTens 6 0 = "sixty"
parseTens 6 u = "sixty " ++ digitToWord u
parseTens 7 0 = "seventy"
parseTens 7 u = "seventy " ++ digitToWord u
parseTens 8 0 = "eighty"
parseTens 8 u = "eighty " ++ digitToWord u
parseTens 9 0 = "ninety"
parseTens 9 u = "ninety " ++ digitToWord u

parseHundreds :: Int -> Int -> Int -> String
parseHundreds 0 t u = parseTens t u
parseHundreds h 0 0 = digitToWord h ++ " hundred"
parseHundreds h t u = digitToWord h ++ " hundred and " ++ parseTens t u

parseThousands :: Int -> Int -> Int -> Int -> String
parseThousands th 0 0 0 = digitToWord th ++ " thousand"
parseThousands th h t u = digitToWord th ++ " thousand " ++ parseHundreds h t u

getDigits :: Int -> [Int]
getDigits = map digitToInt . show

numberInWords :: Int -> String
numberInWords n
		| numDigits == 1 = digitToWord n
		| numDigits == 2 = parseTens (head digits) (digits !! 1)
		| numDigits == 3 = parseHundreds (head digits) (digits !! 1) (digits !! 2)
		| numDigits == 4 = parseThousands (head digits) (digits !! 1) (digits !! 2) (digits !! 3)
		where numDigits = length digits
		      digits = getDigits n

answer = (length . filter isLetter . concat) numbers where
	numbers = map numberInWords [1..1000]
