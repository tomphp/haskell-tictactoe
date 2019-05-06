module TicTacToe.Board where

import Control.Error.Safe (atZ)

data Cell = Empty | Naught | Cross deriving (Eq, Show)

type Board = [Cell]

newBoard :: Board
newBoard = replicate 9 Empty

setCell :: Board -> Cell -> Int -> Board
setCell board cell position = 
  case currentValue of
    Nothing    -> error "Attempting to set a cell which does not exist"
    Just Empty -> take position board ++ cell : drop (succ position) board
    _          -> error "Attempting to set a cell which is not empty"
  where
    currentValue = board `atZ` position
