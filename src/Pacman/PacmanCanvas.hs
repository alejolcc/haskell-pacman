module PacmanCanvas where

import Constants
import Types
import Pacman
import Graphics.Gloss

pacmanSize :: Float
pacmanSize = widthBox / 2

pacmanPlayer :: Int -> Picture
pacmanPlayer 1 = Color yellow $ arcSolid 45 315 pacmanSize
pacmanPlayer 2 = Color yellow $ arcSolid 23 337 pacmanSize
pacmanPlayer 3 = Color yellow $ arcSolid 10 350 pacmanSize
pacmanPlayer 4 = Color yellow $ arcSolid 25 345 pacmanSize
pacmanPlayer _ = Color yellow $ arcSolid 45 315 pacmanSize

rotatePacman :: Movement -> Picture -> Picture
rotatePacman U p = rotate (-90) p
rotatePacman L p = rotate 180 p
rotatePacman D p = rotate (-270) p
rotatePacman _ p = p

renderPacman :: Pacman -> Picture
renderPacman pacman = pacmanPicture
  where
    (x, y) = location pacman
    orientation = direction pacman
    pacman' = rotatePacman orientation $ pacmanPlayer $ mouth pacman
    pacmanPicture = translate x y $ pacman'
