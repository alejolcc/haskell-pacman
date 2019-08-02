module State where

import Types
import qualified Pacman
import qualified Ghost

testdungeon :: Dungeon
testdungeon = reverse [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]


data GameState = Game
  {
    dungeon :: Dungeon,
    bufferMov :: Movement,
    pacman :: Pacman.Pacman,
    ghosts :: [Ghost.Ghost]
  }

instance Show GameState where
  show Game{ bufferMov=mov , pacman=p, ghosts=g} =
    show mov ++ " " ++ show p

initialState :: GameState
initialState = Game
  {
    dungeon = testdungeon,
    bufferMov = S,
    ghosts = [ghost1, ghost2, ghost3, ghost4],
    pacman = Pacman.initialPacman
  }
  where
    ghost1 = Ghost.setPosition Ghost.initialGhost (12, 3)
    ghost2 = Ghost.setPosition Ghost.initialGhost (13, 3)
    ghost3 = Ghost.setPosition Ghost.initialGhost (14, 3)
    ghost4 = Ghost.setPosition Ghost.initialGhost (15, 3)

--------------------
-- Pacman updates --
--------------------

-- updatePacman :: GameState -> GameState
-- updatePacman game = updatePacmanPos . updatePacmanMov . updateMouth $ game

-- --TODO: Move mouth state of pacman, to pacman module
-- updateMouth :: GameState -> GameState
-- updateMouth game = game'
--     where
--     mouth = pacmanMounth game
--     mouth' = if mouth == 4 then 0 else mouth + 1
--     game' = game {pacmanMounth = mouth'}

-- updatePacmanMov :: GameState -> GameState
-- updatePacmanMov game = newState
--     where 
--     next = bufferMov game
--     newState = updateDir game next
    
-- updatePacmanPos :: GameState -> GameState
-- updatePacmanPos game = newState
--     where 
--     mov = direction $ pacman game
--     newState = movePacman game mov

-- -- Update pacman direction
-- updateDir :: GameState -> Movement -> GameState
-- updateDir game mov = game {pacman=pacman'}
--     where
--     pman = pacman game
--     pos = position pman
--     dir = direction pman
--     dg = dungeon game
--     dir' = if validateMov pos dg mov then mov else dir
--     pacman' = setDirection pman dir' 

-- -- Logic to move pacman on the dungeon
-- movePacman :: GameState -> Movement -> GameState
-- movePacman game mov = game {dungeon=newDg, pacman=pacman'}
--     where
--     pman = pacman game
--     pos = position pman
--     dg = dungeon game
--     newPos = if validateMov pos dg mov then updatePos pos mov else pos
--     pacman' = setPosition pman newPos
--     newDg = eatPill dg newPos

-- -- Validate next move
-- validateMov :: Pos -> Dungeon -> Movement -> Bool
-- validateMov pos dg mov = valid 
--     where 
--     (x, y) = updatePos pos mov
--     valid = elem (x, y) $ getValidPos dg

-- -- Update a player position
-- updatePos :: Pos -> Movement -> Pos
-- updatePos (0, 1) L = (27, 1) 
-- updatePos (27, 1) R = (0, 1) 
-- updatePos (x, y) U = (x, y+1) 
-- updatePos (x, y) D = (x, y-1) 
-- updatePos (x, y) L = (x-1, y) 
-- updatePos (x, y) R = (x+1, y) 
-- updatePos (x, y) _ = (x, y) 

-- --------------------------------------------------------------------------------


-- printState :: GameState -> IO GameState
-- printState game = do 
--                     putStrLn $ show game
--                     return game


-- handleKeys :: Event -> GameState -> IO GameState
-- handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = return game {bufferMov = U}
-- handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = return game {bufferMov = D}
-- handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = return game {bufferMov = L}
-- handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = return game {bufferMov = R}

-- handleKeys _ game = return game
    





eatPill :: Dungeon -> Pos -> Dungeon
eatPill dg p = newdg
    where
    (x, y) = p
    row = dg !! y
    newRow = replace row x Empty
    newdg = replace dg y newRow



-- -- Aux
replace :: [a] -> Int -> a -> [a]
replace xs pos newVal = take pos xs ++ newVal : drop (pos+1) xs
            
-- -- get the space inside of dungeon
-- getSpace :: Dungeon -> Pos -> Space
-- getSpace xs (px, py) =
--     xs !! py !! px

-- getDungeonSize :: Dungeon -> (Int, Int)
-- getDungeonSize dg = (w-1, h-1)
--     where 
--     w = length $ dg !! 0
--     h = length dg

-- -- Get all valid locations on dungeon
-- getValidPos :: Dungeon -> [Pos]
-- getValidPos dg = res
--     where
--     (w, h) = getDungeonSize dg
--     posiblePos = [(x, y) | x <- [0..w], y <- [0..h]]
--     valid = \pos -> Pill == (getSpace dg pos) || Empty == (getSpace dg pos)
--     res = filter valid posiblePos
    
-- ghostSt :: Int -> GameState -> Ghost
-- ghostSt n game = ghost
--     where
--     gSt = ghosts game 
--     ghost = gSt !! n 
