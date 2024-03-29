module IA where

-- TODO: Handle randomness with real random seed

import Algorithm.Search
import qualified Data.Maybe as Maybe
import Constants
import qualified Ghost
import qualified Pacman
import DungeonUtils as DGutils
import Data.List

---------------------
-- AStar Algorithm --
---------------------

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
                                _ -> U
-----------------
-- Ghost Rules --
----------------

updateGhost :: Int -> Ghost.Ghost -> Pacman.Pacman -> [(Int, Int)] -> Int -> Ghost.Ghost
updateGhost gid gh pman valid rand = gh'
  where
    start = Ghost.position gh
    end = case Ghost.mode gh of
      Chase -> selectChaseTarget gid gh pman valid
      Scatter -> selectScatterTarget gid rand valid
      Frightened -> selectFrightenedTarget gid rand pman valid
    valid' = (removeInvalidTiles gh pman valid)
    move = nextMove start end valid'
    gh' = if not (Ghost.isMoving gh) then Ghost.setDirection gh move else gh

-- we have to remove th epacman position from valid list positions if the ghost is frighten
-- also, we have to remove the previous move of the ghost, because they cant go back 
removeInvalidTiles :: Ghost.Ghost -> Pacman.Pacman -> [(Int, Int)] -> [(Int, Int)]
removeInvalidTiles gh pman valid = valid''
  where
    tile = case Ghost.direction gh of
      U -> DGutils.getNeighborTile (Ghost.position gh) D
      D -> DGutils.getNeighborTile (Ghost.position gh) U
      R -> DGutils.getNeighborTile (Ghost.position gh) L
      L -> DGutils.getNeighborTile (Ghost.position gh) R
      S -> DGutils.getNeighborTile (Ghost.position gh) S
    valid' = removeItem tile valid
    valid'' = if Ghost.mode gh == Frightened then
                removeItem (Pacman.position pman) valid'
              else valid'

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
-- 0 - Blinky
-- 1 - Pinky
-- 2 - Inky
-- 3 - Clyde
-- https://gameinternals.com/understanding-pac-man-ghost-behavior

-- TODO: Use a method to generate a loop in a corner for a random dungeon
selectScatterTarget :: Int -> Int -> [(Int, Int)] -> (Int, Int)
selectScatterTarget gid rand valid = DGutils.getRandomTile (gid + rand) valid

selectFrightenedTarget :: Int -> Int -> Pacman.Pacman -> [(Int, Int)] -> (Int, Int)
selectFrightenedTarget gid rand pman valid = DGutils.getRandomTile (gid + rand * rand) awayTiles
  where
    filterClose = \tile -> (dist tile (Pacman.position pman)) > 10
    awayTiles = (filter filterClose valid)

selectChaseTarget :: Int -> Ghost.Ghost -> Pacman.Pacman-> [(Int, Int)] -> (Int, Int)
selectChaseTarget 0 gh pman valid = target
  where
    target = Pacman.position pman

selectChaseTarget 1 gh pman valid = (nextNspaces 4 pos dir valid)
  where
    dir = Pacman.direction pman
    pos = Pacman.position pman

selectChaseTarget 2 gh pman valid = target
  where
    dir = Pacman.direction pman
    pos = Pacman.position pman
    (gx, gy) = Ghost.position gh
    (rx, ry) = (nextNspaces 2 pos dir valid)
    selTile = (rx + (-gx), ry + (-gy))
    target = if elem selTile valid then selTile else pos

selectChaseTarget 3 gh pman valid = target
  where
    ppos = Pacman.position pman
    distance = dist (Ghost.position gh) ppos
    target = if distance > 8 then ppos else (1, 1)

nextNspaces :: Int -> (Int, Int) -> Movement -> [(Int, Int)] -> (Int, Int)
nextNspaces n (x, y) mov valid = if tileList == [] then (x, y) else last tileList
  where
    posList = case mov of
      U -> [(x, y) | y <- [y..y+n]]
      D -> [(x, y) | y <- [y..y-n]]
      L -> [(x, y) | x <- [x..x-n]]
      R -> [(x, y) | x <- [x..x+n]]
      _ -> [(x,y)]
    foo = \tile -> elem tile valid
    tileList = filter foo posList

