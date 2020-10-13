{-# OPTIONS -fno-warn-tabs #-}

module Sokoban where

import qualified Data.Map.Strict as Map

data BoardValue =
	Empty
	| Man
	| Crate
	| Wall
	| Storage
	| StorageAndCrate
	| StorageAndMan
	deriving Eq

instance (Show BoardValue) where
	show Empty = " "
	show Man = "@"
	show Crate = "o"
	show Storage = "."
	show StorageAndCrate = "*"
	show StorageAndMan = "+"
	show Wall = "#"

readBoardValue :: Char -> BoardValue
readBoardValue '#' = Wall
readBoardValue '@' = Man
readBoardValue 'o' = Crate
readBoardValue '.' = Storage
readBoardValue '*' = StorageAndCrate
readBoardValue '+' = StorageAndMan
readBoardValue ' ' = Empty

data GameState = GameState {playerPosition :: Coord , gameBoard :: Board}

type Board = Map.Map Coord BoardValue

printBoard :: Board -> String
printBoard b = foldr1 (\f s -> f ++ "\n" ++ s) $ map (concatMap ( show . snd)) inOrder
	where
		pieces = Map.assocs b
		inOrder = reverse $ rows pieces

rows :: [(Coord, BoardValue)] -> [[(Coord, BoardValue)]]
rows [] = []
rows (e@((x,_), _):ls) = ( e:pre ) : rows pos
	where
		select ((x',_),_) = x'
		(pre,pos) = span (\b -> x == select b) ls

type Coord = (Int, Int)

data Input = Up | Down | DLeft | DRight deriving (Show, Eq, Ord)

add :: Coord -> Input -> Coord
add (x, y) Up     = (x, y+1)
add (x, y) Down   = (x, y-1)
add (x, y) DLeft  = (x-1, y)
add (x, y) DRight = (x+1, y)

combine :: BoardValue -> BoardValue -> BoardValue
combine Man Empty     = Man
combine Man Storage   = StorageAndMan
combine Crate Empty   = Crate
combine Crate Storage = StorageAndCrate

remove :: BoardValue -> BoardValue
remove Man             = Empty
remove Crate           = Empty
remove StorageAndMan   = Storage
remove StorageAndCrate = Storage

move :: Coord -> Input -> Board -> Maybe Board
move coord direction board = do
	here <- Map.lookup coord board
	let next = add coord direction
	board' <- move next direction board
	neighbour <- Map.lookup next board'
	let here' = remove here
	let next' = combine here neighbour
	case here of
		Wall -> Nothing
		Empty -> return board
		_ -> return $ Map.insert coord here' $ Map.insert next next' board'

parse :: String -> Board
parse s = Map.fromList $ concat preDict
	where
		xs = reverse $ lines s
		enumerate = zip [1..]
		numbered = enumerate $ map enumerate xs
		combine' (y, ls) = map (\(x, c) -> ((x,y), c)) ls
		preBoard = map combine' numbered
		preDict = map (map (\(c, bv) -> (c, readBoardValue bv))) preBoard


initialize :: Board -> Maybe GameState
initialize board = do
	let elems = map swap $ Map.assocs board
	pos <- lookup Man elems
	return $ GameState pos board
	where
		swap (a,b) = (b,a)


main :: IO ()
main = do
	boardString <- getContents
	putStr $ printBoard $  parse boardString
