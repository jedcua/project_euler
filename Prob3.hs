import Control.Monad

getPrimeFactorsIO :: Int -> IO [Int]
getPrimeFactorsIO n = filterM isPrimeIO $ getFactorsIO n

getFactorsIO :: Int -> IO [Int]
getFactorsIO n = filterM (isDivisibleIO n) [n,n-1..1]

-- 

isDivisibleIO :: Int -> Int -> IO Bool
isDivisibleIO n k = if n `isDivisible` k
					  then do
						  putStrLn $ "[*] " ++ show n ++ "/" ++ show k
						  return True
					  else do
						  putStrLn $ "[ ] " ++ show n ++ "/" ++ show k
						  return False

isPrimeIO :: Int -> IO Bool
isPrimeIO n = return (isPrime n)

-- Pure Functions
getFactors :: Int -> [Int]
getFactors n = filter (isDivisible n) [n,n-1..1]

isDivisible :: Int -> Int -> Bool
isDivisible n k = n `mod` k == 0

isPrime :: Int -> Bool
isPrime n = getFactors n == [n, 1]
