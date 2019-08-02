module Ghost where

import Types


data Ghost = Ghost
  {
    ghostPos :: Pos,
    ghostDir :: Movement
  } deriving Show


initialGhost :: Ghost
initialGhost = Ghost
  {
    ghostPos = (12, 3),
    ghostDir = S
  }

instance Player Ghost where
  position ghost = ghostPos ghost
  direction ghost = ghostDir ghost
  setDirection = setGhostDirection
  setPosition = setGhostPosition
  
setGhostPosition :: Ghost -> Pos -> Ghost
setGhostPosition ghost pos = ghost {ghostPos=pos}

setGhostDirection :: Ghost -> Movement -> Ghost
setGhostDirection ghost next = ghost {ghostDir=next}