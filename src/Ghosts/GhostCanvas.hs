module GhostCanvas where

import Constants
import Ghost
import Graphics.Gloss

ghostSize :: Float
ghostSize = widthBox / 2

ghostPlayer :: Color -> Picture
ghostPlayer c = Color c $ arcSolid 1 360 ghostSize

deadGhostPlayer :: Picture
deadGhostPlayer = Color white $ thickArc 1 360 ghostSize 3

renderGhost :: Ghost -> Picture
renderGhost ghost = ghostRendered
  where
    (gx, gy) = location ghost
    color = case (Ghost.weak ghost, Ghost.gid ghost) of
      (True, _) -> blue
      (_, 0) -> red
      (_, 1) -> rose
      (_, 2) -> orange
      (_, 3) -> azure
      (_, _) -> green
    ghostRendered = case Ghost.alive ghost of
      True -> translate gx gy $ ghostPlayer color
      False -> translate gx gy $ deadGhostPlayer