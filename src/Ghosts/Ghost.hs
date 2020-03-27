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
  show Ghost{ position=pos, location=loc, gid=gid, speed=spd, weak=w, direction=d, alive=alive} =
    "Ghost " ++ "Pos " ++ show pos ++ " " 
             ++ "Loc " ++ show loc ++ " " 
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
    speed = 5,
    weak = False,
    alive = True,
    direction = S
  }

moveGhost :: Ghost -> Movement -> Ghost
moveGhost gh mov = gh {location=loc, position=(toDungeon loc)}
  where
    (x, y) = location gh
    speedPm = speed gh
    loc = case mov of
      U -> (x, y+speedPm)
      D -> (x, y-speedPm)
      L -> (x-speedPm, y)
      R -> (x+speedPm, y)
      S -> (x, y)

setPosition :: Ghost -> (Int, Int) -> Ghost
setPosition ghost pos = ghost {position=pos}

setDirection :: Ghost -> Movement -> Ghost
setDirection ghost next = ghost {direction=next}

setWeak :: Ghost -> Bool -> Ghost
setWeak gh True = gh {weak=True, speed=5}
setWeak gh False = gh {weak=True, speed=15}

setAlive :: Ghost -> Bool -> Ghost
setAlive gh alive = gh {alive=alive}

isMoving :: Ghost -> Bool
isMoving gh = (round x) `mod` 30 /= 0 || (round y) `mod` 30 /= 0
  where
    (x, y) = location gh