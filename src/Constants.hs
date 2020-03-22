module Constants where

data Movement = U | D | L | R | S  deriving Show
data Space = Pill | Empty | Wall deriving (Show, Eq)

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
toDungeon :: (Float, Float) -> Movement -> (Int, Int)
toDungeon (x, y) mov = 
  case mov of
    U ->(round x `div` round widthBox, round y `div` round heightBox)
    D ->(round x `div` round widthBox, round y `div` round heightBox)
    L ->(round x `div` round widthBox, round y `div` round heightBox)
    R ->(round x `div` round widthBox, round y `div` round heightBox)
    S ->(round x `div` round widthBox, round y `div` round heightBox)