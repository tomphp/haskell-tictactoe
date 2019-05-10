module TicTacToe.UI where

import TicTacToe.Game (GameState)

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: GameState -> m ()
  getPositionInput :: m Int
