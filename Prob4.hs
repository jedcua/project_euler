import Control.Monad

isPalindrome :: Int -> IO Bool
isPalindrome n = if show n == (reverse . show) n
					 then do
						 putStrLn $ "[*] " ++ show n
						 return True
					 else return False

findLargestPalindrome :: [Int] -> [Int] -> IO Int
findLargestPalindrome xs ys = liftM maximum palindromes where
		palindromes = filterM isPalindrome [x*y | x <- xs, y <- ys]


answer = findLargestPalindrome [100..999] [100..999]
