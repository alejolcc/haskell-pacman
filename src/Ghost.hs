module Ghost where

import Types


data Ghost = Ghost
  {
    position :: Pos,
    direction :: Movement
  } deriving Show


initialGhost :: Ghost
initialGhost = Ghost
  {
    position = (12, 3),
    direction = S
  }

  
setPosition :: Ghost -> Pos -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}