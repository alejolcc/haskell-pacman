module Pacman where

import Types

data Pacman = Pacman
  {
    location :: (Float, Float), -- The position on the canvas
    position :: Pos,           -- The box on the dungeon
    speed :: Int,
    mouth :: Int,
    direction :: Movement
  } deriving Show


initialPacman :: Pacman
initialPacman = Pacman
  {
    location = (0, 0),
    position = (1, 1),
    speed = 1,
    mouth = 1,
    direction = S
  }

-- TODO: Add velocity logic
movePacman :: Pacman -> Movement -> Pacman
movePacman pm pos = pm {location=loc}
  where
    (x, y) = location pm
    loc = case pos of
      U -> (x, y+1)
      D -> (x, y-1)
      L -> (x-1, y)
      R -> (x+1, y)
      S -> (x, y)

setPosition :: Pacman -> Pos -> Pacman
setPosition pm pos = pm {position=pos}

setDirection :: Pacman -> Movement -> Pacman
setDirection pm mov = pm {direction=mov}

setSpeed :: Pacman -> Int -> Pacman
setSpeed pm speed = pm {speed=speed}

updateMouth :: Pacman-> Pacman
updateMouth pm = pm {mouth=mouth'}
    where
      mouth' = if (mouth pm) == 4 then 0 else (mouth pm) + 1