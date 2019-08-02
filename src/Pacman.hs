module Pacman where

import Types

data Pacman = Pacman
  {
    position :: Pos,
    speed :: Integer,
    mounth :: Integer,
    direction :: Movement
  } deriving Show


initialPacman :: Pacman
initialPacman = Pacman
  {
    position = (1, 1),
    speed = 1,
    mounth = 1,
    direction = S
  }

setPosition :: Pacman -> Pos -> Pacman
setPosition pm pos = pm {position=pos}

setDirection :: Pacman -> Movement -> Pacman
setDirection pm mov = pm {direction=mov}

setSpeed :: Pacman -> Integer -> Pacman
setSpeed pm speed = pm {speed=speed}