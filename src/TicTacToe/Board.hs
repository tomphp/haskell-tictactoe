module TicTacToe.Board
  ( Board
  , Error(..)
  , contains
  , fromCells
  , getCell
  , lines
  , empty
  , render
  , setCell
  ) where

import Prelude hiding (empty, lines)

import Control.Monad.Except (MonadError, throwError)

import           TicTacToe.Coordinate (Coordinate)
import qualified TicTacToe.Coordinate as Coordinate
import           TicTacToe.Line       (Line(..))

data Error = CellDoesNotExist
           | CellIsNotEmpty
           deriving (Eq, Show)

newtype Board a = Board { getCell :: Coordinate -> Maybe a }

instance Semigroup (Board a) where
  Board b1 <> Board b2 = Board $ combine b1 b2
    where combine c1 c2 coord
            | isJust (c1 coord) = c1 coord
            | otherwise         = c2 coord

instance Monoid (Board a) where
  mempty = empty

instance Eq a => Eq (Board a) where
  b1 == b2 = cells b1 == cells b2

instance Show a => Show (Board a) where
  show b = "fromCells " ++ show (cells b)

empty :: Board a
empty = Board $ const Nothing

singleCell :: Maybe a -> Coordinate -> Board a
singleCell Nothing = const empty
singleCell (Just c)  =
  \setCoord -> Board $ \lookupCoord -> if setCoord == lookupCoord then Just c else Nothing

fromCells :: [Maybe a] -> Board a
fromCells get = mconcat fns
  where
    fns = zipWith singleCell get Coordinate.allCoordinates

setCell :: MonadError Error m => a -> Int -> Board a -> m (Board a)
setCell cell position (Board get) =
  case Coordinate.fromIndex position of
    Nothing    -> throwError CellDoesNotExist
    Just coord -> if isNothing (get coord)
                   then return $ singleCell (Just cell) coord <> Board get
                   else throwError CellIsNotEmpty

contains :: Eq a => Board a -> Maybe a -> Bool
contains b c = c `elem` cells b

cells :: Board a -> [Maybe a]
cells (Board get) = fmap get Coordinate.allCoordinates

lines :: Board a -> [Line a]
lines board = mconcat [rows board, columns board, diagonals board]

rows :: Board a -> [Line a]
rows (Board get) = fmap (Line . fmap get) Coordinate.allRows

columns :: Board a -> [Line a]
columns (Board get) = fmap (Line . fmap get) Coordinate.allColumns

diagonals :: Board a -> [Line a]
diagonals (Board get) = fmap (Line . fmap get) Coordinate.allDiagonals

render :: ([Maybe a] -> b) -> Board a -> b
render r = r . cells
