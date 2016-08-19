-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.


import Control.Monad.State

data Primes = Primes { sumPrimes :: Int , primeList :: [Int] } deriving Show

findPrimes :: Int -> State Primes Int
findPrimes maxPrime = do
		primeData <- get

		let acc = sumPrimes primeData
		    primes = primeList primeData
		    lastPrime = last primes
		    nextPrime = take 1 $ dropWhile (not . isPrime primes) [lastPrime+1..]

		if head nextPrime >= maxPrime
	  		then return lastPrime
			else do
				put $ Primes (acc + head nextPrime) (primes ++ nextPrime)
				findPrimes maxPrime

isPrime :: [Int] -> Int -> Bool
isPrime ps n = null factors where
	factors = filter (\k -> n `mod` k == 0) ps

answer = sumPrimes $ execState (findPrimes 1000) (Primes 0 [2])
