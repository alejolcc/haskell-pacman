module State where

import IA
import System.Random
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Constants
import qualified DungeonUtils as DGutils
import qualified Pacman
import qualified Ghost
import Graphics.Gloss.Interface.IO.Game


data GameState = Game
  {
    lifes :: Int,
    dungeon :: Dungeon,
    bufferMov :: Movement,
    pacman :: Pacman.Pacman,
    ghosts :: Seq.Seq Ghost.Ghost,
    validPos :: [(Int, Int)],
    warpsPos ::[((Int, Int), (Int, Int))],
    globalTimer :: Float,
    energizerTimer :: Float,
    generator :: StdGen,
    config :: Config
  }

instance Show GameState where
  show game = "State "
    ++ "BuffMov "        ++ show (bufferMov game)      ++ " "
    ++ "globalTimer "    ++ show (globalTimer game)    ++ " "
    ++ "energizerTimer " ++ show (energizerTimer game) ++ "\n"
    ++ "Pacman "         ++ show (pacman game)         ++ "\n"
    ++ "Ghosts "         ++ show (ghosts game)         ++ " "

createInitialState :: Config -> GameState
createInitialState config = Game
  {
    lifes           = lifes',
    dungeon         = dungeon',
    bufferMov       = S,
    ghosts          = Seq.fromList $ map createGhost (zip [0, 1, 2, 3] ghosts'),
    pacman          = pacman',
    validPos        = validPos',
    warpsPos        = warpsPos',
    globalTimer     = 0,
    energizerTimer  = 0,
    generator       = mkStdGen seed,
    config          = config
  }
  where
    dungeon'    = configDungeon config
    lifes'      = configLifes config
    ghosts'     = configGhosts config
    pacman'     = Pacman.initialPacman (configPacman config)
    validPos'   = DGutils.getValidPos dungeon'
    warpsPos'   = configWarps config
    seed        = configRandSeed config
    createGhost = \(x, y) -> (Ghost.initialGhost x y)

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
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = return $ createInitialState (config game)
handleKeys _ game = return game

setTimer :: Float -> GameState -> GameState
setTimer advTime game
  | energizerTimer game < 0 = game {globalTimer = globalTimer game + advTime}
  | energizerTimer game >= 0 = game {energizerTimer = energizerTimer game - advTime}

--------------------
-- Game Colitions --
--------------------
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
  True -> game {ghosts=ghosts'}
    where
      ghosts' = Seq.update (Ghost.gid gh) (Ghost.setAlive gh False) (ghosts game)

  False -> game' {lifes=lifes', dungeon=dungeon', generator=mkStdGen seed'}
    where
      lifes' = (lifes game) - 1
      dungeon' = dungeon game
      seed' = (configRandSeed $ config game) * 2
      game' = createInitialState $ config game

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
updatePacman t game =
  slowPacman . handleWarp . updatePacmanPos . updatePacmanDir $ game

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

slowPacman :: GameState -> GameState
slowPacman game
  | energizerTimer game > 0 = game
  | energizerTimer game <= 0 = game {pacman = pacman'}
    where
      pman = pacman game
      pacman' = if not (Pacman.isMoving pman)
                then Pacman.setSpeed pman 5
                else pman

--------------------
-- Ghosts updates --
--------------------

updateGhosts :: Float -> GameState -> GameState
updateGhosts t game =
  enforceGhost . changeGhostMode . (updateGhostTimer t) . updateGhostPos . updateGhostDir $ game

updateGhostDir :: GameState -> GameState
updateGhostDir game = game{ghosts=ghosts', generator=generator'}
  where
    pman = pacman game
    valid = validPos game
    (genRandom, generator') = random $ (generator game) :: (Int, StdGen)
    ghostSeq = ghosts game
    update = \gid gh -> IA.updateGhost gid gh pman valid genRandom
    ghosts' = (Seq.mapWithIndex update ghostSeq)

updateGhostPos :: GameState -> GameState
updateGhostPos game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    update = \_ gh -> Ghost.moveGhost gh (Ghost.direction gh)
    ghosts' = (Seq.mapWithIndex update ghostSeq)

updateGhostTimer :: Float -> GameState -> GameState
updateGhostTimer t game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    updateTimer = \_ gh -> Ghost.setTimer gh (Ghost.timer gh + t)
    ghosts' = (Seq.mapWithIndex updateTimer ghostSeq)

changeGhostMode :: GameState -> GameState
changeGhostMode game
  | energizerTimer game > 0 = game
  | energizerTimer game <= 0 = game {ghosts = ghosts'}
    where
      t = globalTimer game
      ghostSeq = ghosts game
      update = \_ gh -> Ghost.changeMode gh t
      ghosts' = (Seq.mapWithIndex update ghostSeq)

enforceGhost :: GameState -> GameState
enforceGhost game
  | energizerTimer game > 0 = game
  | energizerTimer game <= 0 = game {ghosts = ghosts'}
    where
      ghostSeq = ghosts game
      update = \_ gh -> Ghost.setAlive (Ghost.setWeak gh False) True
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
    valid = validPos game
    warpedPman = Pacman.handleWarp pman warps
    pacman' = if elem pos (plainWarps warps) && elem (Pacman.position warpedPman) valid 
              then warpedPman
              else pman


eatPill :: GameState -> GameState
eatPill game = game {dungeon=dungeon'}
  where
    (x, y) = Pacman.position (pacman game)
    dg = dungeon game
    row = dg !! y
    newRow = DGutils.replace row x Empty
    dungeon' = DGutils.replace dg y newRow

eatSuperPill :: GameState -> GameState
eatSuperPill game = game {ghosts=ghosts'', energizerTimer=eTimer, pacman=pacman'}
  where
    ghostSeq = ghosts game
    pman = pacman game
    weaken = \_ ghost -> Ghost.setWeak ghost True
    setFrightened = \_ ghost -> Ghost.setMode ghost Frightened
    ghosts' = (Seq.mapWithIndex weaken ghostSeq)
    ghosts'' = (Seq.mapWithIndex setFrightened ghosts')
    pacman' = Pacman.setSpeed pman 6

plainWarps :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
plainWarps [(x, y)] = [x, y]
plainWarps ((x, y): xs) = [x, y] ++ plainWarps xs