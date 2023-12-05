module D04 (task1, task2) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Text.Regex.TDFA ((=~))

task1 :: String -> Int
task1 x =
  let parseLine l = map (map read . words) (splitOn "|" $ splitOn ":" l !! 1) :: [[Int]]
      calcPoints [w, n] = (\x -> if x > 0 then 2 ^ (x - 1) else 0) $ length $ intersect w n
   in sum $ map (calcPoints . parseLine) $ lines x

type Submatches = (String, String, String, [String])

data Card = Card {val :: Int, win :: [Int], num_ :: [Int]} deriving (Show, Eq, Ord)

type CardStack = Map.Map Card Int

getCards :: String -> [Card]
getCards x =
  let getCardNum l = (\(_, _, _, [m]) -> read m) (l =~ "([[:digit:]]+):" :: Submatches)
      getNumbers l = map (map read . words) (splitOn "|" $ splitOn ":" l !! 1) :: [[Int]]
   in [Card {val = getCardNum l, win = head $ getNumbers l, num_ = last $ getNumbers l} | l <- lines x]

computeWins :: Card -> CardStack -> [Int]
computeWins c s =
  let numbers = take (length (win c `intersect` num_ c)) [val c + 1, val c + 2 ..]
      multiplier = s Map.! c
   in [x | x <- numbers, _ <- [1 .. multiplier]]

winsToCards :: [Int] -> Map.Map Int Card -> [Card]
winsToCards w m = map (m Map.!) w

updateStack :: CardStack -> [Card] -> CardStack
updateStack = foldr (\c acc -> Map.insert c ((acc Map.! c) + 1) acc)

task2 :: String -> Int
task2 x =
  let cards = getCards x
      cm = Map.fromList $ map (\x -> (val x, x)) cards
      s = Map.fromList $ map (,1) cards
   in sum $ Map.elems $ evalCards cards s cm
  where
    evalCards cc s cm = foldl (\acc c -> updateStack acc $ winsToCards (computeWins c acc) cm) s cc