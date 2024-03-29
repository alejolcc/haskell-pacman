module LevelParser where

import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char 
import DungeonUtils(getSpace)

import Constants

parse :: String -> Config -> IO Config
parse args config = do
  configFile <- readFile args
  let configLines = lines configFile
  let config' = foldr parseDungeon config configLines
  let config'' = plainDungeon . findWarps . findGhosts . findPacman $ config'
  return config''

parseDungeon :: String -> Config -> Config
parseDungeon str config = config {configDungeon = ((configDungeon config)++[(map f str)])}

f :: Char -> Space
f '#' = Wall
f '.' = Pill
f 'O' = SuperPill
f ' ' = Empty
f 'P' = Pman
f 'G' = Gh
f x
  | isNumber x = Warp $ digitToInt x

findPacman :: Config -> Config
findPacman config = config {configPacman = (x, y)}
  where
    dg = configDungeon config
    f1 = \t -> t == Pman
    f2 = \l -> any f1 l
    [y] = findIndices f2 dg
    [x] = findIndices f1 (dg !! y)

findGhosts :: Config -> Config
findGhosts config = config {configGhosts = indices}
  where
    dg = configDungeon config
    f1 = \t -> t == Gh
    f2 = \l -> any f1 l
    ys = findIndices f2 dg
    xs = map (\i -> (i, findIndices f1 (dg !! i))) ys
    indices = [(z, y) | (y, x) <- xs, z <- x]

findWarps :: Config -> Config
findWarps config = config {configWarps = warpPos}
  where
    dg = configDungeon config
    f1 = \t -> isWarp t
    f2 = \l -> any f1 l
    ys = findIndices f2 dg
    xs = map (\i -> (i, findIndices f1 (dg !! i))) ys
    indices = [(getSpace dg (z, y), (z, y)) | (y, x) <- xs, z <- x]
    warpPos = getWarps indices


getWarps :: [(Space, (Int, Int))] -> [((Int, Int), (Int, Int))]
getWarps ws = sorted
  where
    sorted = getWarpPos $ sort ws

getWarpPos :: [(Space, (Int, Int))] -> [((Int, Int), (Int, Int))]
getWarpPos [] = []
getWarpPos [(Warp _, x), (Warp _, y)] = [(x, y)]
getWarpPos ((Warp _, x):((Warp _, y):xs)) = [(x, y)] ++ getWarpPos xs

isWarp :: Space -> Bool
isWarp (Warp _) = True
isWarp _ = False

plainDungeon :: Config -> Config
plainDungeon config = config {configDungeon=dungeon'}
  where
    dg = configDungeon config
    foo_row = \tile -> map g tile
    dungeon' = map foo_row dg

g :: Space -> Space
g Pman = Pill
g Gh = Empty
g (Warp _) = Empty
g x = x