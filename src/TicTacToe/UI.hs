module TicTacToe.UI where

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: m ()
  getPositionInput :: m Int
