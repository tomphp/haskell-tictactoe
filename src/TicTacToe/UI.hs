module TicTacToe.UI where

import TicTacToe.Board (Board)

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board -> m ()
  getPositionInput :: m Int
