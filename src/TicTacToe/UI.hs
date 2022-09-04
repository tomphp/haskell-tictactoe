module TicTacToe.UI where

import AppPrelude

import TicTacToe.Board      (Board)
import TicTacToe.Coordinate (Coordinate)
import TicTacToe.Player     (Player)

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board Player -> m ()
  getPositionInput :: m Coordinate
