module D03 (task1, task2) where

import Data.Set qualified as Set
import GHC.Stack.Types qualified as Set
import Text.Regex.TDFA (getAllMatches, getAllTextMatches, (=~))

type Point = (Int, Int)

getSymbolPoints :: String -> Set.Set Point
getSymbolPoints x = makePoints 0 $ lines x
  where
    makePoints _ [] = Set.empty
    makePoints i (xh : xt) = Set.union (Set.fromList $ linePoints i xh) (makePoints (i + 1) xt)
      where
        linePoints i l = map (\(x, _) -> (x, i)) (getAllMatches (l =~ "([^.0-9])") :: [(Int, Int)])

getAdjacentPoints :: Set.Set Point -> Set.Set Point
getAdjacentPoints s =
  let m = Set.lookupMin s
   in case m of
        Just min_ -> Set.union (Set.fromList $ aP min_) (getAdjacentPoints $ Set.delete min_ s)
        Nothing -> Set.empty
  where
    aP (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

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

isNumberAdjacentToSymbol :: Number_ -> Set.Set Point -> Bool
isNumberAdjacentToSymbol n symbols = False

task1 :: String -> Int
task1 x = 0

task2 :: String -> Int
task2 x = 0