module Ghost where

import Constants


data Ghost = Ghost
  {
    gid :: Int,
    position :: (Int, Int),
    location :: (Float, Float), -- The position on the canvas
    speed :: Float,
    weak :: Bool,
    alive :: Bool,
    direction :: Movement
  }

instance Show Ghost where
  show Ghost{ position=pos, gid=gid, speed=spd, weak=w, direction=d, alive=alive} =
    "Ghost " ++ "Pos " ++ show pos ++ " " 
             ++ "Id " ++ show gid ++ " " 
             ++ "Speed " ++ show spd ++ " " 
             ++ "Weak " ++ show w ++ " " 
             ++ "Dir " ++ show d ++ " "
             ++ "Alive " ++ show alive
             ++ "\n"


initialGhost :: Int -> (Int, Int) -> Ghost
initialGhost gid pos = Ghost
  {
    gid = gid,
    location = toCanvas(pos),
    position = pos,
    speed = 15,
    weak = False,
    alive = True,
    direction = S
  }

setPosition :: Ghost -> (Int, Int) -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}

setWeak :: Ghost -> Bool -> Ghost
setWeak gh True = gh {weak=True, speed=5}
setWeak gh False = gh {weak=True, speed=15}

setAlive :: Ghost -> Bool -> Ghost
setAlive gh alive = gh {alive=alive}