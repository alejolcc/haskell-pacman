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


initialPacman :: Pacman
initialPacman = Pacman
  {
    location = (0, 0),
    position = (1, 1),
    speed = 10,
    mouth = 1,
    direction = S
  }

movePacman :: Pacman -> Movement -> Pacman
movePacman pm pos = updateMouth(pm {location=loc, position=toDungeon(loc)})
  where
    (x, y) = location pm
    speedPm = speed pm
    loc = case pos of
      U -> (x, y+speedPm)
      D -> (x, y-speedPm)
      L -> (x-speedPm, y)
      R -> (x+speedPm, y)
      S -> (x, y)

setPosition :: Pacman -> (Int, Int) -> Pacman
setPosition pm pos = pm {position=pos}

setDirection :: Pacman -> Movement -> Pacman
setDirection pm mov = pm {direction=mov}

setSpeed :: Pacman -> Float -> Pacman
setSpeed pm speed = pm {speed=speed}

updateMouth :: Pacman-> Pacman
updateMouth pm = pm {mouth=mouth'}
    where
      mouth' = if (mouth pm) == 4 then 0 else (mouth pm) + 1