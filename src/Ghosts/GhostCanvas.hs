module GhostCanvas where

import Constants
import Ghost
import Graphics.Gloss

ghostSize :: Float
ghostSize = widthBox / 2

ghostPlayer :: Picture
ghostPlayer = Color red $ arcSolid 1 360 ghostSize

renderGhost :: Ghost -> Picture
renderGhost ghost = ghostRendered
  where
    (gx, gy) = location ghost
    ghostRendered = translate gx gy $ ghostPlayer