import Control.Monad.State

type Primes = [Int]

-- Its a prime number if it isn't divisible by previous prime numbers
isPrime :: Primes -> Int -> Bool
isPrime primes n = null primeDivisibles where 
	primeDivisibles = filter (isDivisible n) primes
	isDivisible n k = n `mod` k == 0

findPrimes :: [Int] -> State Primes Int
findPrimes [x] = return x
findPrimes (x:xs) = do
		primes <- get
		if isPrime primes x
	    	then do
				let newPrimes = x:primes
				put newPrimes
				findPrimes xs
			else findPrimes xs


findPrimesUntil :: Int -> [Int]
findPrimesUntil n = execState (findPrimes [3,5..n]) [3,2]
