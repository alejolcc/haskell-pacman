module IA where

import Algorithm.Search
import qualified Data.Maybe as Maybe
import Constants
import qualified Ghost
import qualified Pacman
import DungeonUtils as DGutils

neighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

nextMove :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Movement
nextMove start end invalid = firstMove moves start
  where
    isWall = \pos -> not (elem pos invalid)
    (_, moves) = case aStar (neighbors `pruning` isWall) dist (dist end) (== end) start of
      Nothing -> (0, [])
      path -> Maybe.fromJust path

firstMove :: [(Int, Int)] -> (Int, Int) -> Movement
firstMove [] pos = S
firstMove ((x, y):xs) (x', y') = case (x-x', y-y') of
                                (-1, 0) -> L
                                (1, 0) -> R
                                (0, 1) -> U
                                (0, -1) -> D
                                _ -> S


updateGhost :: Int -> Ghost.Ghost -> Pacman.Pacman -> [(Int, Int)] -> Ghost.Ghost
updateGhost gid gh pman valid = gh'
  where
    start = Ghost.position gh
    end = selectTarget gid gh pman valid
    move = nextMove start end valid
    gh' = if not (Ghost.isMoving gh) then Ghost.setDirection gh move else gh

-- 0 - Blinky
-- 1 - Pinky
-- 2 - Inky
-- 3 - Clyde
-- https://gameinternals.com/understanding-pac-man-ghost-behavior

selectTarget :: Int -> Ghost.Ghost -> Pacman.Pacman-> [(Int, Int)] -> (Int, Int)
selectTarget 0 gh pman valid = target
  where
    target = Pacman.position pman

selectTarget 1 gh pman valid = (nextNspaces 4 pos dir)
  where
    dir = Pacman.direction pman
    pos = Pacman.position pman

selectTarget 2 gh pman valid = target
  where
    dir = Pacman.direction pman
    pos = Pacman.position pman
    (gx, gy) = Ghost.position gh
    (rx, ry) = (nextNspaces 2 pos dir)
    target = (rx + (-gx), ry + (-gy))

selectTarget 3 gh pman valid = target
  where
    ppos = Pacman.position pman
    distance = dist (Ghost.position gh) ppos
    target = if distance > 8 then ppos else (1, 1)

-- TODO: get only valid positions
nextNspaces :: Int -> (Int, Int) -> Movement -> (Int, Int)
nextNspaces n pos dir = case (pos, dir) of
  ((x, y), U) -> (x, y+n)
  ((x, y), D) -> (x, y-n)
  ((x, y), L) -> (x-n, y+n)
  ((x, y), R) -> (x+n, y)
  _ -> pos

-- getRandomTile :: Int -> [(Int, Int)] -> (Int, Int)
-- getRandomTile n = (x, y)
--     xs = take 10 $ randomRs (1, 360) (mkStdGen n)
--     ys = take 10 $ randomRs (1, 1000) (mkStdGen n)