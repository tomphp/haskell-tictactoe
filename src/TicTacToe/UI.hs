module TicTacToe.UI where

import TicTacToe.Actions (GameState)

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: GameState -> m ()
  getPositionInput :: m Int
