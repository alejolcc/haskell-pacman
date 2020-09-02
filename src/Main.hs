module Main(main) where

import Canvas
import LevelParser
import Constants
import System.Environment
import System.Exit
import Data.Char
import System.IO.Unsafe

main :: IO ()
main = do
    args    <- getArgs
    config  <- parseArgs args defaultConfig
    print args
    play config

-- Config ---------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  {
    configDungeon = [],
    configLifes   = 3,
    configRandSeed= 42
  }

parseArgs :: [String] -> Config -> IO Config
parseArgs args config
        | []    <- args
        = return config

        | "-lifes" : lifes : rest <- args
        = parseArgs rest $ config { configLifes = read lifes }

        | "-seed" : seed: rest <- args
        , all isDigit seed
        = parseArgs rest $ config { configRandSeed = read seed }

        | "-level" : level : rest <- args
        = parseArgs rest $ unsafePerformIO $ parse level config

        | otherwise
        = do    printUsage
                exitWith $ ExitFailure 1

printUsage :: IO ()
printUsage
 = putStr $ unlines
        [ "haskell-pacman [flags]"
        , "    -lifes      <NAT>        Lifes of the pacman"
        , "    -seed       <NAT>        Initial seed for random generator"
        , "    -level      <STRING>     Path to level file config"
        , ""
        ]