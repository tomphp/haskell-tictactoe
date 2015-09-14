module TicTacToe.Board where

data Cell = Empty | Naught | Cross deriving (Eq, Show)

type Board = [Cell]

type CellIndex = Int

newBoard :: Board
newBoard = replicate 9 Empty -- take 9 $ repeat Empty

setCell :: Board -> Cell -> CellIndex -> Board
setCell board cell position = case currentValue of
  Empty -> take position board ++ cell : drop (succ position) board
  _     -> error "Attempting to set a cell which is not empty"
  where currentValue = board!!position
