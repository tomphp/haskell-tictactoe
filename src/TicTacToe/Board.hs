{-# LANGUAGE FlexibleContexts #-}

module TicTacToe.Board
  ( Board(..)
  , BoardError(..)
  , Cell(..)
  , cells
  , column
  , diagonal
  , lines
  , new
  , row
  , setCell
  ) where

import Prelude hiding (lines)

import Control.Monad.Except (MonadError, throwError)

import Control.Error.Safe (atZ)
import Data.List.Split (chunksOf)

data Cell = Empty | Naught | Cross deriving (Eq, Show)

data BoardError = CellDoesNotExist | CellIsNotEmpty deriving (Eq)

instance Show BoardError where
  show CellDoesNotExist =  "Attempting to set a cell which does not exist"
  show CellIsNotEmpty   =  "Attempting to set a cell which is not empty"

newtype Board = Board [Cell] deriving (Eq)

new :: Board
new = Board $ replicate 9 Empty

setCell :: MonadError BoardError m => Cell -> Int -> Board -> m Board
setCell cell position (Board cs) =
  case currentValue of
    Nothing    -> throwError CellDoesNotExist
    Just Empty -> return $ Board newCells
    _          -> throwError CellIsNotEmpty
  where
    newCells = take (pred position) cs ++ cell : drop position cs
    currentValue = cs `atZ` pred position

cells :: Board -> [Cell]
cells (Board cs) = cs

lines :: Board -> [[Cell]]
lines board = concat [ rows board, columns board, diagonals board ]

row :: Int -> Board -> Maybe [Cell]
row rowNum board = rows board `atZ` pred rowNum

rows :: Board -> [[Cell]]
rows (Board cs) = chunksOf 3 cs

column :: Int -> Board -> Maybe [Cell]
column colNum board = columns board `atZ` pred colNum

columns :: Board -> [[Cell]]
columns = transformedLines indexes
  where indexes = [0, 3, 6, 1, 4, 7, 2, 5, 8]

diagonal :: Int -> Board -> Maybe [Cell]
diagonal colNum board = diagonals board `atZ` pred colNum

diagonals :: Board -> [[Cell]]
diagonals = transformedLines indexes
  where indexes = [0, 4, 8, 2, 4, 6]

transformedLines :: [Int] -> Board -> [[Cell]]
transformedLines indexes (Board cs) =
  case transformElements indexes cs of
    Just r  -> chunksOf 3 r
    Nothing -> error "should never ever happen"

transformElements :: [Int] -> [a] -> Maybe [a]
transformElements indexes xs = mapM (atZ xs) indexes

instance Show Board where
  show (Board board) = unpack rendered
    where
      rendered    = intercalate "\n---------\n" formatted
      formatted   = map formatRow rs
      rs          = chunksOf 3 parsedBoard
      parsedBoard = zipWith (curry cellToChar) board [1..9]
      formatRow   = unwords . intersperse "|"

cellToChar :: (Cell, Int) -> Text
cellToChar (Naught, _)     = "O"
cellToChar (Cross, _)      = "X"
cellToChar (Empty, number) = tshow number
