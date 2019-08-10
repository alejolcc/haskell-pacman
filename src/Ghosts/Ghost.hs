module Ghost where

import Constants


data Ghost = Ghost
  {
    position :: (Int, Int),
    direction :: Movement
  } deriving Show


initialGhost :: Ghost
initialGhost = Ghost
  {
    position = (12, 3),
    direction = S
  }

setPosition :: Ghost -> (Int, Int) -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}