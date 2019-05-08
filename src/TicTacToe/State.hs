module TicTacToe.State
  ( TheState
  , board
  , player
  , new
  , updateBoard
  , updatePlayer
  )
  where

import           TicTacToe.Board (Board)
import qualified TicTacToe.Board as Board
import           TicTacToe.Player (Player(Crosses))

data TheState = TheState { board :: Board
                         , player :: Player
                         }

new :: TheState
new = TheState { board = Board.new, player = Crosses }

updatePlayer :: (Player -> Player) -> TheState -> TheState
updatePlayer update state = state { player = update (player state) }

updateBoard :: (Board -> Board) -> TheState -> TheState
updateBoard update state = state { board = update  (board state) }
