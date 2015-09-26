module TicTacToe.AI.Executor (execute) where

import TicTacToe.AI.ParseTree
import TicTacToe.Board
import TicTacToe.GameLogic
import TicTacToe.Player

type CellTest = CellIndex -> Move -> Bool
type MoveTest = Move -> Bool

isEmpty :: CellTest
isEmpty cell (board, _, _) = case board !! cell of
    Empty -> True
    _     -> False

isCurrent :: CellTest
isCurrent testCell (_, _, playCell) = playCell == testCell

isForPlayer :: Cell -> Player -> Bool
isForPlayer cell player = case player of
  Naughts -> cell == Naught
  Crosses -> cell == Cross

isMine :: CellTest
isMine testCell (board, player, _) = isForPlayer (board !! testCell) player

isTheirs :: CellTest
isTheirs testCell (board, player, _) = isForPlayer (board !! testCell) $ switchPlayer player

chooseTest :: Move -> Condition -> MoveTest
chooseTest move condition = case condition of
  CurrentCellIsCond cell  -> isCurrent cell
  CellIsEmptyCond cell    -> isEmpty cell
  CellIsNotEmptyCond cell -> not . isEmpty cell
  CellIsMineCond cell     -> isMine cell
  CellIsTheirsCond cell   -> isTheirs cell

chooseBranch :: Move -> ParseTree -> ParseTree -> MoveTest -> ParseTree
chooseBranch move succeed fail check =
  if check move then succeed
                else fail

execute :: ParseTree -> Move -> Int
execute (Result value) _                        = value
execute (IfAndNode condition success fail) move = execute branch move
  where test   = chooseTest move condition
        branch = chooseBranch move success fail test
