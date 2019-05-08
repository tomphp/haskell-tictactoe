module TicTacToe.State
  ( TheState
  , currentPlayer
  , newState
  , theBoard
  )
  where

import           TicTacToe.Board (Board)
import qualified TicTacToe.Board as Board
import           TicTacToe.Player (Player(Crosses))

data TheState = TheState { theBoard :: Board
                         , currentPlayer :: Player
                         }

newState :: TheState
newState = TheState { theBoard = Board.new, currentPlayer = Crosses }
