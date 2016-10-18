module Prob28 where

-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is 
-- formed as follows:

            -- 21 22 23 24 25
            -- 20  7  8  9 10
            -- 19  6  1  2 11
            -- 18  5  4  3 12
            -- 17 16 15 14 13

-- It can be verified that the sum of the numbers on the diagonals is 101.
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

import qualified Data.Matrix as M
import Control.Monad.State

type Pos = (Int, Int)
data Direction = L | D | R | U deriving (Enum, Show)
type Vector = (Pos, Direction)

width :: Int
width = 5

height :: Int
height = 5

matrix :: M.Matrix Int
matrix = M.matrix width height (const 0)

move :: Pos -> Direction -> Pos
move (x,y) U = (x  , y-1)
move (x,y) L = (x-1, y  )
move (x,y) D = (x  , y+1)
move (x,y) R = (x+1, y  )

vacant :: Pos -> Direction -> M.Matrix Int -> Bool
vacant pos dir mat = xBounded && yBounded && unset where
    (x',y')  = move pos dir
    xBounded = x' > 0 && x' < width  + 1 
    yBounded = y' > 0 && y' < height + 1 
    unset    = M.getElem x' y' mat == 0

switchDir :: Direction -> Direction
switchDir L = D
switchDir D = R
switchDir R = U
switchDir U = L
