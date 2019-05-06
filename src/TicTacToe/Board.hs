module TicTacToe.Board
  ( Board(..)
  , Cell(..)
  , cells
  , newBoard
  , setCell
  , drawBoard
  ) where

import Control.Error.Safe (atZ)
import Data.List.Split (chunksOf)

data Cell = Empty | Naught | Cross deriving (Eq, Show)

newtype Board = Board [Cell]

newBoard :: Board
newBoard = Board $ replicate 9 Empty

cells :: Board -> [Cell]
cells (Board cs) = cs

setCell :: Board -> Cell -> Int -> Board
setCell (Board board) cell position = 
  case currentValue of
    Nothing    -> error "Attempting to set a cell which does not exist"
    Just Empty -> Board $ take position board ++ cell : drop (succ position) board
    _          -> error "Attempting to set a cell which is not empty"
  where
    currentValue = board `atZ` position

drawBoard :: Board -> Text
drawBoard (Board board) =
  rendered
    where
      rendered    = intercalate "\n---------\n" formatted
      formatted   = map formatRow rows
      rows        = chunksOf 3 parsedBoard
      parsedBoard = zipWith (curry cellToChar) board [1..9]
      formatRow   = unwords . intersperse "|"

cellToChar :: (Cell, Int) -> Text
cellToChar (Naught, _)     = "O"
cellToChar (Cross, _)      = "X"
cellToChar (Empty, number) = tshow number
