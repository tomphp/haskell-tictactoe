module TicTacToe.UI where

import TicTacToe.Board (Board, Cell)

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board Cell -> m ()
  getPositionInput :: m Int
