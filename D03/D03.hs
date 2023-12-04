module D03 (task1, task2) where

import Data.Set qualified as Set
import Text.Regex.TDFA (getAllMatches, getAllTextMatches, (=~))

type Point = (Int, Int)

getCharPoints :: Int -> String -> [String] -> Set.Set Point
getCharPoints _ _ [] = Set.empty
getCharPoints i rx (xh : xt) = Set.union (Set.fromList $ linePoints i rx xh) (getCharPoints (i + 1) rx xt)
  where
    linePoints i rx l = map (\(x, _) -> (x, i)) (getAllMatches (l =~ (rx :: String)) :: [(Int, Int)])

getSymbolPoints :: String -> Set.Set Point
getSymbolPoints x = getCharPoints 0 "([^.0-9])" $ lines x

getGearPoints :: String -> Set.Set Point
getGearPoints x = getCharPoints 0 "([*])" $ lines x

getAdjacentPoints :: Set.Set Point -> Set.Set Point
getAdjacentPoints s =
  let m = Set.lookupMin s
   in case m of
        Just min_ -> Set.union (Set.fromList $ aP min_) (getAdjacentPoints $ Set.delete min_ s)
        Nothing -> Set.empty
  where
    aP (x, y) = [(xx, yy) | xx <- [x - 1 .. x + 1], yy <- [y - 1 .. y + 1]]

data Number_ = Number_ {val :: Int, points :: [Point]} deriving (Show)

getNumbers :: String -> [Number_]
getNumbers x = createNumbers 0 $ lines x
  where
    createNumbers _ [] = []
    createNumbers i (xh : xt) =
      let rx = "([[:digit:]]+)"
          texts = getAllTextMatches (xh =~ rx) :: [String]
          spans = getAllMatches (xh =~ rx) :: [(Int, Int)]
       in zipWith (\t s -> Number_ {val = read t, points = spanToPoints i s}) texts spans ++ createNumbers (i + 1) xt
      where
        spanToPoints i (idx, len) = [(x, i) | x <- [idx .. (idx + len - 1)]]

task1 :: String -> Int
task1 x =
  let numbers = getNumbers x
      neighborhood = getAdjacentPoints $ getSymbolPoints x
   in sum $ map val $ filter (`isNumberInPoints` neighborhood) numbers
  where
    isNumberInPoints n p = not $ null $ Set.intersection p (Set.fromList $ points n)

task2 :: String -> Int
task2 x =
  let numbers = getNumbers x
      gears = getGearPoints x
   in sum $ map (multiplyNumbers . getAdjacentNumbers numbers) $ Set.toList gears
  where
    multiplyNumbers x = if length x == 2 then product $ map val x else 0
    getAdjacentNumbers numbers p =
      let neighborhood = getAdjacentPoints $ Set.fromList [p]
       in filter (\n -> not $ null $ Set.intersection (Set.fromList $ points n) neighborhood) numbers