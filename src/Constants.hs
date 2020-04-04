module Constants where

data Movement = U | D | L | R | S  deriving Show
data Space = Pill | Empty | Wall | SuperPill deriving (Show, Eq)
data GhostMode = Chase | Scatter | Frightened deriving (Show, Eq)

type Dungeon = [[Space]]

dgPosX :: Float
dgPosX = (-600)

dgPosY :: Float
dgPosY = (-300)

fps :: Int
fps = 20

widthBox :: Float
widthBox = 30

heightBox :: Float
heightBox = 30

-- Map a canvas position to dg box
toDungeon :: (Float, Float) -> (Int, Int)
toDungeon (x, y) = (round (x / widthBox), round (y / heightBox))

toCanvas :: (Int, Int) -> (Float, Float)
toCanvas (x, y) = (fromIntegral x * widthBox, fromIntegral y * heightBox)