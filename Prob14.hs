-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting 
-- numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.


import Data.List (maximumBy)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : if even n
					then collatz (n `div` 2)
					else collatz (3 * n + 1)

collatzNumbers :: [(Int, Int)]
collatzNumbers = map (\n -> (n, countCollatz n)) [1..1000000] where
	countCollatz = length . collatz

answer = maximumBy compareByChainLength collatzNumbers where
	compareByChainLength x y = snd x `compare` snd y
