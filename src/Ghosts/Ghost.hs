module Ghost where

import Constants


data Ghost = Ghost
  {
    position :: (Int, Int),
    location :: (Float, Float), -- The position on the canvas
    direction :: Movement
  } deriving Show


initialGhost :: Ghost
initialGhost = Ghost
  {
    position = (12, 3),
    location = (5, 5),
    direction = S
  }

setPosition :: Ghost -> (Int, Int) -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}