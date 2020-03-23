module Ghost where

import Constants


data Ghost = Ghost
  {
    position :: (Int, Int),
    location :: (Float, Float), -- The position on the canvas
    speed :: Float,
    weak :: Bool,
    direction :: Movement
  }

instance Show Ghost where
  show Ghost{ position=pos, speed=spd, weak=w, direction=d} =
    "Ghost " ++ show pos ++ " " ++ show spd ++ " " ++ show w ++ " "++ show d


initialGhost :: (Int, Int) -> Ghost
initialGhost pos = Ghost
  {
    location = toCanvas(pos),
    position = pos,
    speed = 15,
    weak = False,
    direction = S
  }

setPosition :: Ghost -> (Int, Int) -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}

setWeak :: Bool -> Ghost -> Ghost
setWeak True gh = gh {weak=True, speed=5}
setWeak False gh = gh {weak=True, speed=15}