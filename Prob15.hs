-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
-- there are exactly 6 routes to the bottom right corner.
-- How many such routes are there through a 20×20 grid?

import Prelude hiding (Right)
import Control.Monad (replicateM)
import Control.Monad.List

data Direction = Down | Right deriving (Eq, Show)

type Route = [Direction]
type Pos = (Int, Int)
type GridSize = Int

makeRoutes :: GridSize -> [Route]
makeRoutes n = do
		a <- replicateM (n*2) [Down, Right]
		guard (isValidRoute n a)
		return a

isValidRoute :: GridSize -> Route -> Bool
isValidRoute n routes = length downs == n && length rights == n where
	downs  = filter (Down ==) routes
	rights = filter (Right ==) routes
