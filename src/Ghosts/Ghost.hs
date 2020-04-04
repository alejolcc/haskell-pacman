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
    direction :: Movement,
    mode :: GhostMode,
    timer :: Float
  }

instance Show Ghost where
  show gh = "Ghost "
              ++ "Pos "   ++ show (position gh)   ++ " "
              ++ "Loc "   ++ show (location gh)   ++ " "
              ++ "Id "    ++ show (gid gh)        ++ " "
              ++ "Speed " ++ show (speed gh)      ++ " "
              ++ "Weak "  ++ show (weak gh)       ++ " "
              ++ "Dir "   ++ show (direction gh)  ++ " "
              ++ "Alive " ++ show (alive gh)      ++ " "
              ++ "Mode"   ++ show (mode gh)       ++ "\n"


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
setWeak gh False = gh {weak=False, speed=5}

setAlive :: Ghost -> Bool -> Ghost
setAlive gh alive = gh {alive=alive}

isMoving :: Ghost -> Bool
isMoving gh = (round x) `mod` 30 /= 0 || (round y) `mod` 30 /= 0
  where
    (x, y) = location gh

setTimer :: Ghost -> Float -> Ghost
setTimer gh t = case alive gh of
  True -> gh {timer = t}
  False -> gh {timer = 0}

-- The ghosts start out in Scatter mode, and there are four waves of Scatter/Chase alternation defined,
-- after which the ghosts will remain in Chase mode indefinitely (until the timer is reset).
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
setMode :: Ghost -> GhostMode -> Ghost
setMode gh m = gh {mode = m}

changeMode :: Ghost -> Float -> Ghost
changeMode gh t
  | t < 7             = setMode gh Scatter
  | t > 7  && t < 27  = setMode gh Chase
  | t > 27 && t < 34  = setMode gh Scatter
  | t > 34 && t < 54  = setMode gh Chase
  | t > 54 && t < 59  = setMode gh Scatter
  | t > 59 && t < 79  = setMode gh Chase
  | t > 79 && t < 84  = setMode gh Scatter
  | t > 84            = setMode gh Chase