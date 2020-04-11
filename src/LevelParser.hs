module LevelParser where

import System.IO
import Control.Monad
import Data.List

import Constants

-- TODO: Do the parse more generic, ¿use instance?

parseArgs :: String -> Config -> IO Config
parseArgs args config = do
  configFile <- readFile args
  let configLines = lines configFile
  let config' = foldr parseConfLine config configLines
  -- print qwe
  return config'

parseConfLine :: String -> Config -> Config
parseConfLine str config
  | "Lifes"  `isPrefixOf` str = parseLifes str config
  | "Ghost"  `isPrefixOf` str = parseGhost str config
  | "Pacman" `isPrefixOf` str = parsePacman str config
  | "Warps"  `isPrefixOf` str = parseWarps str config

parseConfLine str config = parseDungeon str config

parseWarps :: String -> Config -> Config
parseWarps str config = config {configWarps = value'}
  where
    [_, value] = words str
    value' = read value :: [((Int, Int), (Int, Int))]

parseLifes :: String -> Config -> Config
parseLifes str config = config {configLifes = value'}
  where
    [_, value] = words str
    value' = read value :: Int


parseGhost :: String -> Config -> Config
parseGhost str config = config {configGhosts = value'}
  where
    [_, value] = words str
    value' = read value :: [(Int, Int)]

parsePacman :: String -> Config -> Config
parsePacman str config = config {configPacman = value'}
  where
    [_, value] = words str
    value' = read value :: (Int, Int)

parseDungeon :: String -> Config -> Config
parseDungeon str config = config {configDungeon = (configDungeon config) ++ [(map f str)]}

f :: Char -> Space
f '#' = Wall
f '.' = Pill
f 'P' = SuperPill
f ' ' = Empty