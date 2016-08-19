-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

import Control.Monad.List

pythags = do
		z <- [1..]
		x <- [1..z]
		y <- [x..z]
		guard (x^2 + y^2 == z^2 && x + y + z == 1000)
		return (x, y, z)

answer = x * y * z where
	(x, y, z) = (head . take 1) pythags
