{-# OPTIONS -fno-warn-tabs #-}

module Sokoban where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

data BoardValue =
	Empty
	| Crate
	| Wall
	| Storage
	| StorageAndCrate
	deriving (Eq, Show)

data GameState = GameState {playerPosition :: Coord , gameBoard :: Board}

type Board = Map.Map Coord BoardValue

type Coord = (Int, Int)

data Input = Up | Down | DLeft | DRight deriving (Show, Eq, Ord)

add :: Coord -> Input -> Coord
add (x, y) Up     = (x, y+1)
add (x, y) Down   = (x, y-1)
add (x, y) DLeft  = (x-1, y)
add (x, y) DRight = (x+1, y)


canMoveThrough :: BoardValue -> Bool
canMoveThrough Crate                 = True
canMoveThrough Storage               = True
canMoveThrough StorageAndCrate       = True
canMoveThrough _                     = False

canMove :: Board -> Coord -> Input -> Bool
canMove board current direction = fromMaybe False (Map.lookup next board >>= \p -> Just (canMoveThrough p)) && canMove board next direction
	where
		next = add current direction

move :: GameState -> Input -> GameState
move (GameState player board) direction
	| canMove board player direction = undefined
	| otherwise = GameState player board

type Game = [Input]

runGame :: GameState -> Game -> GameState
runGame = foldl move

main :: IO ()
main = undefined
