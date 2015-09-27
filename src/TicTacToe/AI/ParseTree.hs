module TicTacToe.AI.ParseTree where

import TicTacToe.Board

data Condition = CurrentCellIsCond CellIndex
               | CellIsEmptyCond CellIndex
               | CellIsNotEmptyCond CellIndex
               | CellIsMineCond CellIndex
               | CellIsTheirsCond CellIndex deriving (Show, Eq)

data ParseTree = IfAndNode Condition ParseTree ParseTree
               | Result Int deriving (Show, Eq)

