
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 
-- without any remainder. What is the smallest positive number that is evenly divisible 
-- by all of the numbers from 1 to 20?

multipleOf1To20 :: Int -> Bool
multipleOf1To20 n = length divisibles == 20 where
	divisibles = takeWhile (\k -> n `mod` k == 0) [1..20]

findLowestMultiple :: Int -> Int
findLowestMultiple 0 = findLowestMultiple 20
findLowestMultiple n = if multipleOf1To20 n
						   then n
						   else findLowestMultiple (n + 20)

