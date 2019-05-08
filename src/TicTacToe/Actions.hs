module TicTacToe.Actions where

import TicTacToe.Board (Board)
import TicTacToe.Player (Player)

class Monad m => Actions m where
  player       :: m Player
  board        :: m Board
  switchPlayer :: m ()
  setCell      :: Int -> m ()
