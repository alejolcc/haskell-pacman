module Canvas where

import State
import Types
import Constants
import PacmanCanvas
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


window :: Display
-- window = InWindow "Nice Window" (200, 200) (10, 10)
window = FullScreen

background :: Color
background = black

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

-- Render game
render :: GameState -> IO Picture
render game = return $ translate dgPosX dgPosY $ pictures [dg, pacman']
  where
    dg = mkDungeon $ dungeon game
    -- Pacman
    pacman' = renderPacman $ pacman game
    -- Ghost
    -- ghost' = renderGhosts game

update :: Float -> GameState -> IO GameState
update seconds game = printState (updateState game seconds)

paint = playIO window background fps initialState render handleKeys update