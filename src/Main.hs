module Main(main) where

import Canvas
import LevelParser
import Constants

testdungeon :: Dungeon
testdungeon = reverse [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,SuperPill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Pill,Wall],[Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Wall,Wall,Pill,Pill,Pill,Pill,Pill,Pill,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Pill,Wall,Wall,Wall,Wall,Wall,Wall],[Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill,Pill],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

testWarps :: [((Int, Int), (Int, Int))]
testWarps = [((-1, 1), (27, 1)), ((28, 1), (0, 1))]

main :: IO ()
main = do
    config  <- parseArgs "test.txt" defaultConfig

    -- text <- readFile "test.txt"
    -- let l = lines text
    -- let res = map foo l
    -- print res
    print config
    paint2 config

-- Config ---------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  {
    configDungeon = testdungeon,
    -- configDungeon = [],
    configLifes   = 3,
    configGhosts  = [(12, 3), (13, 3), (14, 3), (15, 3)],
    configPacman  = (1, 1),
    configWarps   = [((-1, 1), (27, 1)), ((28, 1), (0, 1))]
  }