{-# OPTIONS -fno-warn-tabs #-}

module Sokoban where

import Prelude hiding (Either(..))
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

data Input = Up | Down | Left | Right deriving (Show, Eq, Ord)

add :: Coord -> Input -> Coord
add (x, y) Up                    = (x, y+1)
add (x, y) Down                  = (x, y-1)
add (x, y) Left                  = (x-1, y)
add (x, y) Right                 = (x+1, y)

collideWith :: BoardValue -> BoardValue -> Maybe (BoardValue, BoardValue)
collideWith Empty _                         = Nothing
collideWith Wall _                          = Nothing
collideWith Storage _                       = Nothing
collideWith Crate Empty                     = Just (Empty           , Crate)
collideWith Crate Crate                     = Just (Empty           , Crate)
collideWith Crate Wall                      = Just (Crate           , Wall)
collideWith Crate Storage                   = Just (Empty           , StorageAndCrate)
collideWith Crate StorageAndCrate           = Just (Empty           , StorageAndCrate)
collideWith StorageAndCrate Empty           = Just (Storage         , Crate)
collideWith StorageAndCrate Crate           = Just (Storage         , Crate)
collideWith StorageAndCrate Wall            = Just (StorageAndCrate , Wall)
collideWith StorageAndCrate Storage         = Just (Storage         , StorageAndCrate)
collideWith StorageAndCrate StorageAndCrate = Just (Storage         , StorageAndCrate)

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
