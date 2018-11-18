{-# OPTIONS -fno-warn-tabs #-}

module Sokoban where

import Prelude hiding (Either(..))


data Board = Board {walls :: [Coord], crates :: [Coord], man :: Coord, storage :: [Coord], bMax :: (Int, Int)} deriving (Show)

displayBoard :: Board -> String
displayBoard b = unlines . map (map func) $ coords
	where 	(maxX, maxY)	= bMax b
		coords				= [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
		func c
			| b `isStorage` c && b `isCrate` c	= '*'
			| b `isStorage` c && c == man b		= '+'
			| c == man b						= '@'
			| b `isCrate` c						= 'o'
			| b `isWall` c						= '#'
			| b `isStorage` c					= '.'
			| otherwise							= ' '


printBoard :: Board -> IO ()
printBoard = putStrLn . displayBoard

readInput :: String -> Input
readInput "h" = Left
readInput "j" = Down
readInput "k" = Up
readInput "l" = Right
readInput s		= error s

-- Example Board:
-- #####
-- #.o@#
-- #####

exampleBoard :: Board
exampleBoard = Board {
	walls 	= [(x,0) | x <- [0..4]] ++ [(x,2) | x <- [0..4]] ++ [(0,1),(4,1)],
	crates 	= [(2,1)],
	man		= (3,1),
	storage	= [(1,1)],
	bMax	= (4,2)
}


type Coord = (Int, Int)

data Input = Up | Down | Left | Right deriving (Show, Eq, Ord)


add :: Coord -> Input -> Coord
add (x, y) Up		= (x, y+1)
add (x, y) Down		= (x, y-1)
add (x, y) Left		= (x-1, y)
add (x, y) Right	= (x+1, y)

isWall :: Board -> Coord -> Bool
isWall board c	= c `elem` walls board

isCrate :: Board -> Coord -> Bool
isCrate board c	= c `elem` crates board

isStorage :: Board -> Coord -> Bool
isStorage board c = c `elem` storage board

isEmpty :: Board -> Coord -> Bool
isEmpty board c	= not (isWall board c || isCrate board c || isStorage board c)



move :: Input -> Board -> Board
move direction board
	| isEmpty board newPos	= board{man = newPos}
	| isCrate board newPos && (isEmpty board newPos' || isStorage board newPos')	= board{crates = moveCrate newPos newPos', man = newPos}
	| otherwise 		= board
	where	pos 	= man board
		newPos		= add pos direction
		newPos'		= add newPos direction
		moveCrate old new	= new : filter (old/=) (crates board)


main :: IO ()
main = do
		putStrLn "Welcome to Sokoban"
		f exampleBoard

f :: Board -> IO ()
f board = do
			printBoard board
			c <- getLine
			if c == "q" then putStrLn "quit" else f $ move (readInput c) board
