module Canvas where

import State
import Types
-- import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

dgPosX :: Float
dgPosX = (-600)

dgPosY :: Float
dgPosY = (-300)

fps :: Int
fps = 20

widthBox :: Float
widthBox = 30

heightBox :: Float
heightBox = 30

window :: Display
-- window = InWindow "Nice Window" (200, 200) (10, 10)
window = FullScreen

background :: Color
background = black

pacmanSize :: Float
pacmanSize = widthBox / 2

-- pacmanPlayer :: Picture
-- pacmanPlayer = Color yellow $ arcSolid 45 320 pacmanSize

pacmanPlayer :: Int -> Picture
pacmanPlayer 1 = Color yellow $ arcSolid 45 315 pacmanSize
pacmanPlayer 2 = Color yellow $ arcSolid 23 337 pacmanSize
pacmanPlayer 3 = Color yellow $ arcSolid 10 350 pacmanSize
pacmanPlayer 4 = Color yellow $ arcSolid 25 345 pacmanSize
pacmanPlayer _ = Color yellow $ arcSolid 45 315 pacmanSize

ghostPlayer :: Picture
ghostPlayer = Color red $ arcSolid 1 180 pacmanSize

-- Draw a box with a pills | wall | empty space 
mkBox :: Space -> Picture
mkBox Wall =  color blue $ rectangleSolid widthBox heightBox
mkBox Empty =  color black $ rectangleSolid widthBox heightBox
mkBox Pill =  pillsBox
  where
    pill = color white $ circleSolid (heightBox / 10)
    box = color black $ rectangleSolid widthBox heightBox
    pillsBox = pictures [box, pill] 

-- Draw a complete row of a dungeon
mkRow :: [Space] -> Picture
mkRow spaces = pictures row
  where
    pics = zip [0..] $ map mkBox spaces
    foo = \(x, y) -> translate (x*widthBox) 0 $ y
    row = map foo pics

-- Draw a Dungeon
mkDungeon :: Dungeon -> Picture
mkDungeon dg = pictures rowList
  where
    rows = zip [0..] $ map mkRow $ dg 
    foo = \(x, y) -> translate 0 (x*heightBox) $ y
    rowList = map foo rows

rotatePacman :: Movement -> Picture -> Picture
rotatePacman U p = rotate (-90) p
rotatePacman L p = rotate 180 p
rotatePacman D p = rotate (-270) p
rotatePacman _ p = p

-- Render game
render :: GameState -> IO Picture
render game = return $ translate dgPosX dgPosY $ pictures [dg, pacman', ghost']
  where
    dg = mkDungeon $ dungeon game 
    -- Pacman
    pacman' = renderPacman game
    -- Ghost
    ghost' = renderGhosts game
    
renderPacman :: GameState -> Picture
renderPacman game = pacman1
  where
    mouth = pacmanMounth game
    (x, y) = position $ pacman game
    orientation = direction $ pacman game
    pacman' = rotatePacman orientation $ pacmanPlayer mouth
    pacman1 = translate (fromIntegral x * widthBox) (fromIntegral y * heightBox) $ pacman'

renderGhosts :: GameState -> Picture
renderGhosts game = res
  where
    ghostList = ghosts game
    pics = map renderGhost ghostList
    res = pictures pics

renderGhost :: (Player a) => a -> Picture
renderGhost ghost = ghostRendered
  where
    (gx, gy) = position ghost
    ghostRendered = translate (fromIntegral gx * widthBox) (fromIntegral gy * heightBox) $ ghostPlayer

update :: Float -> GameState -> IO GameState
update seconds game = printState . updatePacman $ game


paint = playIO window background fps initialState render handleKeys update
-- paint2 = display window background ghostPlayer