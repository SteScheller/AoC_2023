module D02 (task1, task2) where

import Data.List.Split (splitOn)
import System.IO ()
import Text.Regex.TDFA ((=~))

data Cubes = Cubes {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {id_ :: Int, games :: [Cubes]} deriving (Show)

type Submatches = (String, String, String, [String])

getGroups :: Submatches -> [String]
getGroups (_, _, _, g) = g

parseLine :: String -> Game
parseLine x = Game {id_ = getId x, games = getGames x}
  where
    getId x =
      let (_, _, _, [m]) = (x =~ "Game ([[:digit:]]+):" :: Submatches)
       in read m :: Int
    getGames x =
      let a = tail . tail $ dropWhile (/= ':') x
          games = splitOn ";" a
       in map parseCubes games
      where
        parseCubes x =
          let rg = getGroups (x =~ "([[:digit:]]+) red" :: Submatches)
              r = if null rg then 0 else read $ head rg
              gg = getGroups (x =~ "([[:digit:]]+) green" :: Submatches)
              g = if null gg then 0 else read $ head gg
              bg = getGroups (x =~ "([[:digit:]]+) blue" :: Submatches)
              b = if null bg then 0 else read $ head bg
           in Cubes {red = r, green = g, blue = b}

task1 :: String -> Int
task1 x = sum (map id_ (filter checkGame (map parseLine $ lines x)))
  where
    maxCubes (Cubes {red = r, green = g, blue = b}) =
      let maxRed = 12; maxGreen = 13; maxBlue = 14
       in r <= maxRed && g <= maxGreen && b <= maxBlue
    checkGame (Game {games = gg}) = all maxCubes gg

task2 :: String -> Int
task2 x = sum (map (computePower . parseLine) $ lines x)
  where
    computePower x =
      let mr = maximum $ map red $ games x
          mg = maximum $ map green $ games x
          mb = maximum $ map blue $ games x
       in mr * mg * mb
