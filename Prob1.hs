multiples3or5 x
	| x `mod` 3 == 0 = True
	| x `mod` 5 == 0 = True
	| otherwise 	 = False

answer = sum $ filter multiples3or5 [1..999]
