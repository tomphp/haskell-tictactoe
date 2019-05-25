module TicTacToe.UI where

import TicTacToe.Board  (Board)
import TicTacToe.Player (Player)

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board Player -> m ()
  getPositionInput :: m Int
