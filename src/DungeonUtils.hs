module DungeonUtils where

import Constants
import System.Random

replace :: [a] -> Int -> a -> [a]
replace xs pos newVal = take pos xs ++ newVal : drop (pos+1) xs

getValidPos :: Dungeon -> [(Int, Int)]
getValidPos dg = res
    where
      (w, h) = getDungeonSize dg
      posiblePos = [(x, y) | x <- [0..w], y <- [0..h]]
      valid = \pos -> Pill == (getSpace dg pos) || Empty == (getSpace dg pos) || SuperPill == (getSpace dg pos)
      res = filter valid posiblePos

getDungeonSize :: Dungeon -> (Int, Int)
getDungeonSize dg = (w-1, h-1)
    where
      w = length $ dg !! 0
      h = length dg

getSpace :: Dungeon -> (Int, Int) -> Space
getSpace xs (px, py) =
    xs !! py !! px

getRandomTile :: Int -> [(Int, Int)] -> (Int, Int)
getRandomTile n tiles = tiles !! index
  where
    limit = length tiles
    (rand, _) = random (mkStdGen n)
    index = rand `mod` limit

-- TODO: Why this function doesn work?
-- getRandomTile :: Int -> [(Int, Int)] -> (Int, Int)
-- getRandomTile n tiles = tiles !! index
--   where
--     limit = length tiles
--     (index, _) = (randomR (1, limit) (mkStdGen n))
