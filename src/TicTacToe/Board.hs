module TicTacToe.Board
  ( Board
  , Error(..)
  , Cell(..)
  , column
  , contains
  , diagonal
  , fromCells
  , getCell
  , lines
  , empty
  , row
  , render
  , setCell
  ) where

import Prelude hiding (empty, lines)

import Control.Monad.Except (MonadError, throwError)

import Control.Error.Safe (atZ)

data Cell = Empty | O | X deriving (Eq, Show)

data Error = CellDoesNotExist | CellIsNotEmpty deriving (Eq, Show)

newtype Board = Board { getCell :: Coordinate -> Cell }

instance Semigroup Board where
  Board b1 <> Board b2 = Board $ combine b1 b2
    where combine :: (Coordinate -> Cell) -> (Coordinate -> Cell) -> Coordinate -> Cell
          combine c1 c2 coord
            | c1 coord /= Empty = c1 coord
            | otherwise         = c2 coord

instance Monoid Board where
  mempty = empty

instance Eq Board where
  b1 == b2 = cells b1 == cells b2

instance Show Board where
  show b = "fromCells " ++ show (cells b)

-- Row Col
data Coordinate = Coordinate Int Int deriving (Eq)

boardCols :: Int
boardCols = 3

boardRows :: Int
boardRows = 3

indexToCoord :: Int -> Maybe Coordinate
indexToCoord i
  | r < 1 || r > boardCols = Nothing
  | c < 1 || c > boardCols = Nothing
  | otherwise              = Just $ Coordinate r c
  where r = (i - 1) `div` boardCols + 1
        c = (i - 1) `rem` boardCols + 1

empty :: Board
empty = Board $ const Empty

singleCell :: Cell -> Coordinate -> Board
singleCell c coord1 =
  Board $ \coord2 -> if coord1 == coord2 then c else Empty

fromCells :: [Cell] -> Board
fromCells cs = mconcat fns
  where
    fns :: [Board]
    fns = zipWith singleCell cs allCoords

setCell :: MonadError Error m => Cell -> Int -> Board -> m Board
setCell cell position (Board cs) =
  case indexToCoord position of
    Nothing    -> throwError CellDoesNotExist
    Just coord -> if cs coord == Empty
                   then return $ singleCell cell coord <> Board cs
                   else throwError CellIsNotEmpty

contains :: Board -> Cell -> Bool
contains b c = c `elem` cells b

cells :: Board -> [Cell]
cells (Board cs) = map cs allCoords

allCoords :: [Coordinate]
allCoords = [ Coordinate r c | r <- [1..boardCols], c <- [1..boardRows] ]

lines :: Board -> [[Cell]]
lines board = concat [ rows board, columns board, diagonals board ]

row :: Int -> Board -> Maybe [Cell]
row rowNum board = rows board `atZ` pred rowNum

rows :: Board -> [[Cell]]
rows (Board cs) = [ [ cs (Coordinate r c) | c <- [1..boardCols]] | r <- [1..boardRows] ]
 
column :: Int -> Board -> Maybe [Cell]
column colNum board = columns board `atZ` pred colNum

columns :: Board -> [[Cell]]
columns (Board cs) = [ [ cs (Coordinate r c) | r <- [1..boardRows]] | c <- [1..boardCols] ]

diagonal :: Int -> Board -> Maybe [Cell]
diagonal colNum board = diagonals board `atZ` pred colNum

diagonals :: Board -> [[Cell]]
diagonals (Board cs) = [ map (cs . uncurry Coordinate) [(1, 1), (2, 2), (3, 3)]
                       , map (cs . uncurry Coordinate) [(1, 3), (2, 2), (3, 1)]
                       ]

render :: ([Cell] -> Text) -> Board -> Text
render r = r . cells
