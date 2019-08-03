module Types where

data Movement = U | D | L | R | S  deriving Show
data Space = Pill | Empty | Wall deriving (Show, Eq)

type Pos = (Int, Int) 
type Dungeon = [[Space]]