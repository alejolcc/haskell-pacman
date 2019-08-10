module GhostCanvas where

import Constants
import Ghost
import Graphics.Gloss

ghostSize :: Float
ghostSize = widthBox / 2

ghostPlayer :: Picture
ghostPlayer = Color red $ arcSolid 1 180 ghostSize

renderGhost :: Ghost -> Picture
renderGhost ghost = ghostRendered
  where
    (gx, gy) = position ghost
    ghostRendered = translate (fromIntegral gx * widthBox) (fromIntegral gy * heightBox) $ ghostPlayer
