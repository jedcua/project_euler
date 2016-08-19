fibo 0 = 1
fibo 1 = 2
fibo n = fibo (n-1) + fibo (n-2)

answer = sum $ filter even $ takeWhile (< 4000000) $ map fibo [0..]
