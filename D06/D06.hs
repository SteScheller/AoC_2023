module D06 (task1, task2) where

import Data.List.Split (splitOn)

doesWin :: Int -> Int -> Int -> Bool
doesWin timeHold timeMax record = (timeMax - timeHold) * timeHold > record

numWaysToBeat :: Int -> Int -> Int
numWaysToBeat timeMax record = length $ filter (\x -> doesWin x timeMax record) [1 .. timeMax - 1]

task1 :: String -> Int
task1 x = product $ zipWith numWaysToBeat times distances
    where
        getNumbers l = map read $ words $ splitOn ":" l !! 1 :: [Int]
        times = getNumbers $ lines x !! 0
        distances = getNumbers $ lines x !! 1

task2 :: String -> Int
task2 x = numWaysToBeat times distances
    where
        getConcatenatedNumber l = read $ concat $ words $ splitOn ":" l !! 1 :: Int
        times = getConcatenatedNumber $ lines x !! 0
        distances = getConcatenatedNumber $ lines x !! 1
