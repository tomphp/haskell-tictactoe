{-# LANGUAGE TemplateHaskell #-}

module TicTacToe.State (State(..), board, player, new) where

import Control.Lens (makeLenses)

import           TicTacToe.Board  (Board)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

data State = State { _board  :: Board Player
                   , _player :: Player
                   }

makeLenses ''State

new :: State
new = State { _board = Board.empty, _player = X }
