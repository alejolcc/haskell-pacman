module Pacman where

import Types
import Graphics.Gloss.Interface.IO.Game

data Pacman = Pacman
  {
    pacmanPos :: Pos,
    pacmanDir :: Movement
  } deriving Show


initialPacman :: Pacman
initialPacman = Pacman
  {
    pacmanPos = (1, 1),
    pacmanDir = S
  }


instance Player Pacman where
  position = pacmanPos 
  direction = pacmanDir 
  setDirection = setPacmanDirection
  setPosition = setPacmanPosition

setPacmanPosition :: Pacman -> Pos -> Pacman
setPacmanPosition pm pos = pm {pacmanPos=pos}

setPacmanDirection :: Pacman -> Movement -> Pacman
setPacmanDirection pm mov = pm {pacmanDir=mov}