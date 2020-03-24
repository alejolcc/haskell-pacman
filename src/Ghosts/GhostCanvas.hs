module GhostCanvas where

import Constants
import Ghost
import Graphics.Gloss

ghostSize :: Float
ghostSize = widthBox / 2

ghostPlayer :: Color -> Picture
ghostPlayer c = Color c $ arcSolid 1 360 ghostSize

renderGhost :: Ghost -> Picture
renderGhost ghost = ghostRendered
  where
    (gx, gy) = location ghost
    color = case (Ghost.weak ghost, Ghost.alive ghost) of
      (_, False) -> black
      (True, _) -> blue
      (_, _) -> red
    ghostRendered = translate gx gy $ ghostPlayer color