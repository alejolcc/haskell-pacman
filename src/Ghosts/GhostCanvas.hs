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
    color = case (Ghost.weak ghost, Ghost.alive ghost, Ghost.gid ghost) of
      (_, False, _) -> black
      (True, _, _) -> blue
      (_, _, 0) -> red
      (_, _, 1) -> rose
      (_, _, 2) -> orange
      (_, _, 3) -> blue
      (_, _, _) -> green
    ghostRendered = translate gx gy $ ghostPlayer color