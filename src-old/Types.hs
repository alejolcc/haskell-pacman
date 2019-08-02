module Types where

class Player pj where
  position :: pj -> Pos
  direction :: pj -> Movement
  setPosition :: pj -> Pos -> pj
  setDirection :: pj -> Movement -> pj
  
data Movement = U | D | L | R | S  deriving Show
data Space = Pill | Empty | Wall deriving (Show, Eq)

type Pos = (Int, Int) 
type Dungeon = [[Space]]
  
  