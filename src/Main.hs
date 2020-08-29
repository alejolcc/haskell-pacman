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
    -- configDungeon = testdungeon,
    configDungeon = [],
    configLifes   = 3,
    configWarps   = [((27, 14), (0, 14))]
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
        [ "quazicrystal [flags]"
        , "    -fullscreen              Run full screen"
        , "    -window     sizeX sizeY  Run in a window                     (default 800, 600)"
        , "    -zoom       <NAT>        Pixel replication factor            (default 5)"
        , "    -scale      <NAT>        Feature size of visualisation       (default 30)"
        , "    -degree     <NAT>        Number waves to sum for each point  (default 5)"
        , ""
        , " You'll want to run this with +RTS -N to enable threads" ]