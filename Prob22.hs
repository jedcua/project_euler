module Prob22 where

-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over 
-- five-thousand first names, begin by sorting it into alphabetical order. Then working out the 
-- alphabetical value for each name, multiply this value by its alphabetical position in the list 
-- to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 
-- 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score 
-- of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?
import Data.List (elemIndex, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Monad.State

type Letter = Char

letters :: String
letters = ['A'..'Z']

value :: Letter -> Int
value l = (+1) . fromJust $ l `elemIndex` letters

nameScore :: Int -> [Letter] -> Int
nameScore pos ls = pos * sum values where
    values = map value ls

getScore :: String -> State Int Int
getScore name = do
        pos <- get
        let score = nameScore pos name
        modify (+1)
        return score

parseNames :: FilePath -> IO [String]
parseNames file = do
        names <- readFile file
        let names' = splitOn "," names
            names'' = map read names' :: [String]
        return $ sort names''

answer :: IO Int
answer = do
    names <- parseNames "res/p022_names.txt"
    let score = evalState (mapM getScore names) 1
    return $ sum score
