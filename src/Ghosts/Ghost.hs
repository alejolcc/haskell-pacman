module Ghost where

import Constants

data GhostMode = Chase | Scatter | Frightened

data Ghost = Ghost
  {
    gid :: Int,
    position :: (Int, Int),
    location :: (Float, Float), -- The position on the canvas
    speed :: Float,
    weak :: Bool,
    alive :: Bool,
    direction :: Movement,
    mode :: GhostMode,
    timer :: Float
  }

instance Show Ghost where
  show Ghost{ position=pos, location=loc, gid=gid, speed=spd, weak=w, direction=d, alive=alive, timer=t} =
    "Ghost " ++ "Pos " ++ show pos ++ " " 
             ++ "Loc " ++ show loc ++ " " 
             ++ "Id " ++ show gid ++ " " 
             ++ "Speed " ++ show spd ++ " " 
             ++ "Weak " ++ show w ++ " " 
             ++ "Dir " ++ show d ++ " "
             ++ "Alive " ++ show alive ++ " "
             ++ "Timer " ++ show t
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
    direction = S,
    mode = Scatter,
    timer = 0
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

setTimer :: Ghost -> Float -> Ghost
setTimer gh t = case alive gh of
  True -> maybeChangeMode (gh {timer = t}) 7
  False -> maybeChangeMode (gh {timer = 0}) 0

-- The ghosts start out in Scatter mode, and there are four waves of Scatter/Chase alternation defined, 
-- after which the ghosts will remain in Chase mode indefinitely (until the timer is reset). 
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
maybeChangeMode :: Ghost -> Float-> Ghost
maybeChangeMode gh t
  | timer gh < 7                    = gh {mode = Scatter}
  | timer gh > 7 && timer gh < 27   = gh {mode = Chase}
  | timer gh > 27 && timer gh < 34  = gh {mode = Scatter}
  | timer gh > 34 && timer gh < 54  = gh {mode = Chase}
  | timer gh > 54 && timer gh < 59  = gh {mode = Scatter}
  | timer gh > 59 && timer gh < 79  = gh {mode = Chase}
  | timer gh > 79 && timer gh < 84  = gh {mode = Scatter}
  | timer gh > 84                   = gh {mode = Chase}
