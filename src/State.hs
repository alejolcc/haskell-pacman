module State where

import IA
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Constants
import qualified DungeonUtils as DGutils
import qualified Pacman
import qualified Ghost
import Graphics.Gloss.Interface.IO.Game

testdungeon :: Dungeon
testdungeon = reverse [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,SuperPill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

testWarps :: [((Int, Int), (Int, Int))]
testWarps = [((-1, 1), (27, 1)), ((28, 1), (0, 1))]

data GameState = Game
  {
    lifes :: Int,
    dungeon :: Dungeon,
    bufferMov :: Movement,
    pacman :: Pacman.Pacman,
    ghosts :: Seq.Seq Ghost.Ghost,
    validPos :: [(Int, Int)],
    warpsPos ::[((Int, Int), (Int, Int))],
    timer :: Float
  }

instance Show GameState where
  show Game{ bufferMov=mov , pacman=p, ghosts=g, lifes=l, timer=t} =
    "BuffMov " ++ show mov ++ " " ++ "Lifes " ++ show l ++ "\n" 
               ++ show p ++ "\n"
               ++ show g
               ++ show t

initialState :: GameState
initialState = Game
  {
    lifes = 3,
    dungeon = testdungeon,
    bufferMov = S,
    ghosts = Seq.fromList [ghost0, ghost1, ghost2, ghost3],
    pacman = Pacman.initialPacman,
    validPos = DGutils.getValidPos testdungeon,
    warpsPos = testWarps,
    timer = 0
  }
  where
    ghost0 = Ghost.initialGhost 0 (12, 3)
    ghost1 = Ghost.initialGhost 1 (13, 3)
    ghost2 = Ghost.initialGhost 2 (14, 3)
    ghost3 = Ghost.initialGhost 3 (15, 3)

updateState :: GameState -> Float -> GameState
updateState game seconds =
  (setTimer seconds) . resolveColitions . (updateGhosts seconds) . (updatePacman seconds) $ game

printState :: GameState -> IO GameState
printState game = do
                    putStrLn $ show game
                    return game


handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = return game {bufferMov = U}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = return game {bufferMov = D}
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = return game {bufferMov = L}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = return game {bufferMov = R}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = return initialState
handleKeys _ game = return game

setTimer :: Float -> GameState -> GameState
setTimer advTime game = game {timer = timer game + advTime}

resolveColitions :: GameState -> GameState
resolveColitions game = colPacmanGhost . colPacmanPill $ game

colPacmanPill :: GameState -> GameState
colPacmanPill game = game'
  where
    pos = Pacman.position (pacman game)
    dg = dungeon game
    game' = case DGutils.getSpace dg pos of
      SuperPill -> eatSuperPill . eatPill $ game
      _         -> eatPill game

colPacmanGhost :: GameState -> GameState
colPacmanGhost game = game'
  where
    pman = pacman game
    gh = getColision game
    game' = if Maybe.isNothing gh then game else pacmanVsGhost pman (Maybe.fromJust gh) game

pacmanVsGhost :: Pacman.Pacman -> Ghost.Ghost -> GameState -> GameState
pacmanVsGhost pman gh game = case Ghost.weak gh of
                              False -> initialState {lifes=(lifes game) - 1, dungeon = (dungeon game)}
                              True -> game {ghosts=ghosts'}
                                where
                                  ghosts' = Seq.update (Ghost.gid gh) (Ghost.setAlive gh False) (ghosts game)

getColision :: GameState -> Maybe Ghost.Ghost
getColision game = ghost
  where
    pmanPos = Pacman.position (pacman game)
    isColision = \ghost -> Ghost.position ghost == pmanPos
    ghost = List.find isColision (ghosts game)

--------------------
-- Pacman updates --
--------------------

-- TODO: Use timer to handle mouth
updatePacman :: Float -> GameState -> GameState
updatePacman t game = handleWarp . updatePacmanPos . updatePacmanDir $ game

updatePacmanDir :: GameState -> GameState
updatePacmanDir game = newState
    where
      next = bufferMov game
      newState = updateDir game next

-- Update pacman direction
updateDir :: GameState -> Movement -> GameState
updateDir game mov = game {pacman=pacman'}
    where
      pman = pacman game
      valid = validPos game
      pacman' = if not (Pacman.isMoving pman) && validateMov pman mov valid then Pacman.setDirection pman mov else pman

updatePacmanPos :: GameState -> GameState
updatePacmanPos game = game {pacman=pacman'}
    where
      pman = pacman game
      mov = Pacman.direction pman
      valid = validPos game ++ (plainWarps(warpsPos game))
      pacman' = if validateMov pman mov valid then Pacman.movePacman pman mov else pman

validateMov :: Pacman.Pacman -> Movement -> [(Int, Int)] -> Bool
validateMov pman mov positions = res
  where
    (x, y) = Pacman.position pman
    moving = Pacman.isMoving pman
    res = case mov of
      U  -> (elem (x,y+1) $ positions) || moving
      D  -> (elem (x,y-1) $ positions) || moving
      L  -> (elem (x-1,y) $ positions) || moving
      R  -> (elem (x+1,y) $ positions) || moving
      S  -> (elem (x,y) $ positions)

--------------------
-- Ghosts updates --
--------------------

updateGhosts :: Float -> GameState -> GameState
updateGhosts t game =
  (changeGhostMode t) . (updateGhostTimer t) . updateGhostPos . updateGhostDir $ game

updateGhostDir :: GameState -> GameState
updateGhostDir game = game{ghosts=ghosts'}
  where
    pman = pacman game
    valid = validPos game
    ghostSeq = ghosts game
    updateGhosts = \gid gh -> IA.updateGhost gid gh pman valid
    ghosts' = (Seq.mapWithIndex updateGhosts ghostSeq)

updateGhostPos :: GameState -> GameState
updateGhostPos game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    moveGhost = \_ gh -> Ghost.moveGhost gh (Ghost.direction gh)
    ghosts' = (Seq.mapWithIndex moveGhost ghostSeq)

updateGhostTimer :: Float -> GameState -> GameState
updateGhostTimer t game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    updateTimer = \_ gh -> Ghost.setTimer gh (Ghost.timer gh + t)
    ghosts' = (Seq.mapWithIndex updateTimer ghostSeq)

changeGhostMode :: Float -> GameState -> GameState
changeGhostMode t game = game {ghosts = ghosts'}
  where
    ghostSeq = ghosts game
    update = \_ gh -> Ghost.changeMode gh t
    ghosts' = (Seq.mapWithIndex update ghostSeq)

--------------------
------ Aux ---------
--------------------

handleWarp :: GameState -> GameState
handleWarp game = game {pacman=pacman'}
  where
    pman = pacman game
    pos = Pacman.position pman
    warps = warpsPos game
    pacman' = if elem pos (plainWarps warps) then Pacman.handleWarp pman warps else pman

eatPill :: GameState -> GameState
eatPill game = game {dungeon=dungeon'}
  where
    (x, y) = Pacman.position (pacman game)
    dg = dungeon game
    row = dg !! y
    newRow = DGutils.replace row x Empty
    dungeon' = DGutils.replace dg y newRow

eatSuperPill :: GameState -> GameState
eatSuperPill game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    weaken = \_ ghost -> Ghost.setWeak ghost True
    ghosts' = (Seq.mapWithIndex weaken ghostSeq)

plainWarps :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
plainWarps [(x, y)] = [x]
plainWarps ((x, y): xs) = [x] ++ plainWarps xs