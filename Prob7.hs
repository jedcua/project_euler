-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 
-- 6th prime is 13. What is the 10001st prime number?

import Control.Monad.State

type Primes = [Int]

findPrimes :: Int -> State Primes Int
findPrimes nTerm = do
		primes <- get
		let lastPrime = last primes
		if length primes == nTerm
	  		then return lastPrime
			else do
				let nextPrime = take 1 $ dropWhile (not . isPrime primes) [lastPrime+1..]
				put (primes ++ nextPrime)
				findPrimes nTerm

isPrime :: Primes -> Int -> Bool
isPrime ps n = null factors where
	factors = filter (\k -> n `mod` k == 0) ps

answer = evalState (findPrimes 10001) [2]
