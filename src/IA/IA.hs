module IA where

import Algorithm.Search
import qualified Data.Maybe as Maybe
import Constants

neighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

nextMove :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Movement
nextMove start end invalid = getMove moves start
  where
    isWall = \pos -> not (elem pos invalid)
    (_, moves) = case aStar (neighbors `pruning` isWall) dist (dist end) (== end) start of
      Nothing -> (0, [])
      path -> Maybe.fromJust path

getMove [] pos = S
getMove ((x, y):xs) (x', y') = case (x-x', y-y') of
                                (-1, 0) -> L
                                (1, 0) -> R
                                (0, 1) -> U
                                (0, -1) -> D
                                _ -> S

