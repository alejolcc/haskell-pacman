module Constants where

data Movement = U | D | L | R | S  deriving Show
data Space = Pill | Empty | Wall | SuperPill | Pman | Gh | Warp Int deriving (Show, Eq)
data GhostMode = Chase | Scatter | Frightened deriving (Show, Eq)

type Dungeon = [[Space]]

data Config = Config
   {
     configDungeon  :: Dungeon,
     configLifes    :: Int,
     configGhosts   :: [(Int, Int)],
     configPacman   :: (Int, Int),
     configWarps    :: [((Int, Int), (Int, Int))]
   } deriving Show

dgPosX :: Float
dgPosX = (-650)

dgPosY :: Float
dgPosY = (-390)

fps :: Int
fps = 20

widthBox :: Float
widthBox = 30

heightBox :: Float
heightBox = 30

-- Map a canvas position to dg box
toDungeon :: (Float, Float) -> (Int, Int)
toDungeon (x, y) = (truncate (x / widthBox), truncate (y / heightBox))

toCanvas :: (Int, Int) -> (Float, Float)
toCanvas (x, y) = (fromIntegral x * widthBox, fromIntegral y * heightBox)