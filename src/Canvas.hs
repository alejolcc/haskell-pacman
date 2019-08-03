module Canvas where

import State
import Types
import Constants
import PacmanCanvas
import Graphics.Gloss

window :: Display
-- window = InWindow "Nice Window" (200, 200) (10, 10)
window = FullScreen

background :: Color
background = black

-- Render game
render :: GameState -> IO Picture
render game = return $ translate dgPosX dgPosY $ pictures [pacman']
  where
    -- Pacman
    pacman' = renderPacman $ pacman game
    -- Ghost
    -- ghost' = renderGhosts game
