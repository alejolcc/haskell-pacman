module DungeonUtils where

import Constants

getNeighborTile :: (Int, Int) -> Movement -> (Int, Int)
getNeighborTile (x, y) U = (x, y+1)
getNeighborTile (x, y) D = (x, y-1)
getNeighborTile (x, y) L = (x-1, y)
getNeighborTile (x, y) R = (x+1, y)
getNeighborTile (x, y) S = (x, y)

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
getRandomTile rand tiles = tiles !! index
  where
    limit = length tiles
    index = rand `mod` limit
