module Pacman where

import Types

data Pacman = Pacman
  {
    location :: (Float, Float), -- The position on the canvas
    position :: Pos,           -- The box on the dungeon
    speed :: Float,
    mouth :: Int,
    direction :: Movement
  } deriving Show


initialPacman :: Pacman
initialPacman = Pacman
  {
    location = (15, 0),
    position = (1, 1),
    speed = 10,
    mouth = 1,
    direction = S
  }

-- TODO: Add velocity logic
movePacman :: Pacman -> Movement -> Pacman
movePacman pm pos = pm {location=loc}
  where
    (x, y) = location pm
    speedPm = speed pm
    loc = case pos of
      U -> (x, y+speedPm)
      D -> (x, y-speedPm)
      L -> (x-speedPm, y)
      R -> (x+speedPm, y)
      S -> (x, y)

setPosition :: Pacman -> Pos -> Pacman
setPosition pm pos = pm {position=pos}

setDirection :: Pacman -> Movement -> Pacman
setDirection pm mov = pm {direction=mov}

setSpeed :: Pacman -> Float -> Pacman
setSpeed pm speed = pm {speed=speed}

updateMouth :: Pacman-> Pacman
updateMouth pm = pm {mouth=mouth'}
    where
      mouth' = if (mouth pm) == 4 then 0 else (mouth pm) + 1