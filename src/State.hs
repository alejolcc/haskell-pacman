module State where

import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Constants
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
    colision :: Maybe Ghost.Ghost,
    bufferMov :: Movement,
    pacman :: Pacman.Pacman,
    ghosts :: Seq.Seq Ghost.Ghost,
    validPos :: [(Int, Int)],
    warpsPos ::[((Int, Int), (Int, Int))]
  }

instance Show GameState where
  show Game{ bufferMov=mov , pacman=p, ghosts=g, lifes=l, colision=c} =
    "BuffMov " ++ show mov ++ " " ++ "Lifes " ++ show l ++ " " ++ show c++ "\n" 
               ++ show p ++ "\n"
               ++ show g

initialState :: GameState
initialState = Game
  {
    lifes = 3,
    dungeon = testdungeon,
    bufferMov = S,
    colision = Nothing,
    ghosts = Seq.fromList [ghost0, ghost1, ghost2, ghost3],
    pacman = Pacman.initialPacman,
    validPos = getValidPos testdungeon,
    warpsPos = testWarps
  }
  where
    ghost0 = Ghost.initialGhost 0 (12, 3)
    ghost1 = Ghost.initialGhost 1 (13, 3)
    ghost2 = Ghost.initialGhost 2 (14, 3)
    ghost3 = Ghost.initialGhost 3 (15, 3)

updateState :: GameState -> Float -> GameState
updateState game seconds = resolveColitions . updatePacman $ game

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

resolveColitions :: GameState -> GameState
resolveColitions game = colPacmanGhost . colPacmanPill $ game

colPacmanPill :: GameState -> GameState
colPacmanPill game = game'
  where
    pos = Pacman.position (pacman game)
    dg = dungeon game
    game' = case getSpace dg pos of
      SuperPill -> eatSuperPill . eatPill $ game
      _         -> eatPill game

colPacmanGhost :: GameState -> GameState
colPacmanGhost game = game''
  where
    pman = pacman game
    gh = ghostColision game
    game' = if Maybe.isNothing gh then game else pacmanVsGhost pman (Maybe.fromJust gh) game
    game'' = game' {colision=gh} --TODO: Remove line, just for debug

pacmanVsGhost :: Pacman.Pacman -> Ghost.Ghost -> GameState -> GameState
pacmanVsGhost pman gh game = case Ghost.weak gh of
                              False -> initialState {lifes=(lifes game) - 1}
                              True -> game {ghosts=ghosts'}
                                where
                                  ghosts' = Seq.update (Ghost.gid gh) (Ghost.setAlive gh False) (ghosts game)

ghostColision :: GameState -> Maybe Ghost.Ghost
ghostColision game = ghost
  where
    pmanPos = Pacman.position (pacman game)
    isColision = \ghost -> Ghost.position ghost == pmanPos
    ghost = List.find isColision (ghosts game)

--------------------
-- Pacman updates --
--------------------

updatePacman :: GameState -> GameState
updatePacman game = handleWarp . updatePacmanPos . updatePacmanDir $ game

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
    newRow = replace row x Empty
    dungeon' = replace dg y newRow

eatSuperPill :: GameState -> GameState
eatSuperPill game = game {ghosts=ghosts'}
  where
    ghostSeq = ghosts game
    weaken = \_ ghost -> Ghost.setWeak ghost True
    ghosts' = (Seq.mapWithIndex weaken ghostSeq)


-- -- Aux
replace :: [a] -> Int -> a -> [a]
replace xs pos newVal = take pos xs ++ newVal : drop (pos+1) xs

-- Get all valid locations on dungeon
getValidPos :: Dungeon -> [(Int, Int)]
getValidPos dg = res
    where
      (w, h) = getDungeonSize dg
      posiblePos = [(x, y) | x <- [0..w], y <- [0..h]]
      valid = \pos -> Pill == (getSpace dg pos) || Empty == (getSpace dg pos) || SuperPill == (getSpace dg pos)
      res = filter valid posiblePos

getDungeonSize :: Dungeon -> (Int, Int)
getDungeonSize dg = (w-1, h-1)
    where
      w = length $ dg !! 0
      h = length dg

-- get the space inside of dungeon
getSpace :: Dungeon -> (Int, Int) -> Space
getSpace xs (px, py) =
    xs !! py !! px

plainWarps :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
plainWarps [(x, y)] = [x]
plainWarps ((x, y): xs) = [x] ++ plainWarps xs