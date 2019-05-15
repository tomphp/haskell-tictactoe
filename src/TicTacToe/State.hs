{-# LANGUAGE TemplateHaskell #-}

module TicTacToe.State (State, board, player, new) where

import Control.Lens (makeLenses)

import qualified TicTacToe.Board  as Board
import           TicTacToe.Board  (Board)
import           TicTacToe.Player (Player(..))

data State = State { _board :: Board
                   , _player :: Player
                   }

makeLenses ''State

new :: State
new = State { _board = Board.new, _player = Crosses }
