module TicTacToe.Actions (Actions(..), gameIsRunning) where

import           TicTacToe.Board     (Board)
import           TicTacToe.Player    (Player)
import           TicTacToe.GameLogic (GameState(InPlay))
import qualified TicTacToe.GameLogic as GameLogic

class Monad m => Actions m where
  player       :: m Player
  board        :: m Board
  switchPlayer :: m ()
  setCell      :: Int -> m ()

gameIsRunning :: Actions m => m Bool
gameIsRunning = (== InPlay) <$> gameState

gameState :: Actions m => m GameState
gameState = GameLogic.getGameState <$> board

