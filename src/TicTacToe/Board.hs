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

data Cell = O | X deriving (Eq, Show)

data Error = CellDoesNotExist | CellIsNotEmpty deriving (Eq, Show)

newtype Board a = Board { getCell :: Coordinate -> Maybe a }

instance Semigroup (Board a) where
  Board b1 <> Board b2 = Board $ combine b1 b2
    where combine :: (Coordinate -> Maybe a) -> (Coordinate -> Maybe a) -> Coordinate -> Maybe a
          combine c1 c2 coord
            | isJust (c1 coord) = c1 coord
            | otherwise         = c2 coord

instance Monoid (Board a) where
  mempty = empty

instance Eq a => Eq (Board a) where
  b1 == b2 = cells b1 == cells b2

instance Show a => Show (Board a) where
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

empty :: Board a
empty = Board $ const Nothing

singleCell :: Maybe a -> Coordinate -> Board a
singleCell Nothing _      = empty
singleCell c       coord1 =
  Board $ \coord2 -> if coord1 == coord2 then c else Nothing

fromCells :: [Maybe a] -> Board a
fromCells cs = mconcat fns
  where
    fns = zipWith singleCell cs allCoords

setCell :: MonadError Error m => a -> Int -> Board a -> m (Board a)
setCell cell position (Board cs) =
  case indexToCoord position of
    Nothing    -> throwError CellDoesNotExist
    Just coord -> if isNothing (cs coord)
                   then return $ singleCell (Just cell) coord <> Board cs
                   else throwError CellIsNotEmpty

contains :: Eq a => Board a -> Maybe a -> Bool
contains b c = c `elem` cells b

cells :: Board a -> [Maybe a]
cells (Board cs) = map cs allCoords

allCoords :: [Coordinate]
allCoords = [ Coordinate r c | r <- [1..boardCols], c <- [1..boardRows] ]

lines :: Board a -> [[Maybe a]]
lines board = concat [ rows board, columns board, diagonals board ]

row :: Int -> Board a -> Maybe [Maybe a]
row rowNum board = rows board `atZ` pred rowNum

rows :: Board a -> [[Maybe a]]
rows (Board cs) = [ [ cs (Coordinate r c) | c <- [1..boardCols]] | r <- [1..boardRows] ]
 
column :: Int -> Board a -> Maybe [Maybe a]
column colNum board = columns board `atZ` pred colNum

columns :: Board a -> [[Maybe a]]
columns (Board cs) = [ [ cs (Coordinate r c) | r <- [1..boardRows]] | c <- [1..boardCols] ]

diagonal :: Int -> Board a -> Maybe [Maybe a]
diagonal colNum board = diagonals board `atZ` pred colNum

diagonals :: Board a -> [[Maybe a]]
diagonals (Board cs) = [ map (cs . uncurry Coordinate) [(1, 1), (2, 2), (3, 3)]
                       , map (cs . uncurry Coordinate) [(1, 3), (2, 2), (3, 1)]
                       ]

render :: ([Maybe a] -> Text) -> Board a -> Text
render r = r . cells
