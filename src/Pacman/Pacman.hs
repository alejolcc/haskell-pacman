module Pacman where

import Constants

data Pacman = Pacman
  {
    location :: (Float, Float), -- The position on the canvas
    position :: (Int, Int),     -- The box on the dungeon
    speed :: Float,
    mouth :: Int,
    direction :: Movement
  } deriving Show


initialPacman :: (Int, Int) -> Pacman
initialPacman pos = Pacman
  {
    location = toCanvas pos,
    position = pos,
    speed = 5,
    mouth = 1,
    direction = S
  }

movePacman :: Pacman -> Movement -> Pacman
movePacman pm mov = updateMouth $ (pm {location=loc, position=pos})
  where
    (x, y) = location pm
    speedPm = speed pm
    loc = case mov of
      U -> (x, y+speedPm)
      D -> (x, y-speedPm)
      L -> (x-speedPm, y)
      R -> (x+speedPm, y)
      S -> (x, y)
    pm' = pm {location=loc}
    pos = if not (isMoving pm') then toDungeon loc else position pm

setPosition :: Pacman -> (Int, Int) -> Pacman
setPosition pm pos = pm {position=pos}

setDirection :: Pacman -> Movement -> Pacman
setDirection pm mov = pm {direction=mov}

setSpeed :: Pacman -> Float -> Pacman
setSpeed pm speed = pm {speed=speed}

handleWarp :: Pacman -> [((Int, Int), (Int, Int))] -> Pacman
handleWarp pm warps = pm {location=loc', position=pos'}
  where
    mov = direction pm
    pos' = getWarpMap (position pm) mov warps
    loc' = toCanvas pos'

getWarpMap :: (Int, Int) -> Movement -> [((Int, Int),(Int, Int))] -> (Int, Int)
getWarpMap x m (((x1, y1), (x2, y2)) : xs)
  | x == (x1, y1) =
  case m of
    U -> (x2, y2 + 1)
    D -> (x2, y2 - 1)
    R -> (x2 + 1, y2)
    L -> (x2 - 1, y2)

getWarpMap x m (((x1, y1), (x2, y2)) : xs)
  | x == (x2, y2) =
  case m of
    U -> (x1, y1 + 1)
    D -> (x1, y1 - 1)
    R -> (x1 + 1, y1)
    L -> (x1 - 1, y1)

getWarpMap x m (y:ys) = getWarpMap x m ys

updateMouth :: Pacman-> Pacman
updateMouth pm = pm {mouth=mouth'}
    where
      mouth' = if (mouth pm) == 4 then 0 else (mouth pm) + 1

isMoving :: Pacman -> Bool
isMoving pm = (round x) `mod` 30 /= 0 || (round y) `mod` 30 /= 0
  where
    (x, y) = location pm