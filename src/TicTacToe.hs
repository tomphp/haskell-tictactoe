module TicTacToe (run) where 

import Control.Monad.Loops (whileM_)
import Control.Monad.State.Strict (evalStateT)

import           TicTacToe.Actions   (Actions)
import qualified TicTacToe.Actions   as Actions
import           TicTacToe.Game      (runGame)
import           TicTacToe.GameLogic (GameState(..))
import qualified TicTacToe.GameLogic as GameLogic
import           TicTacToe.UI        (UI)
import qualified TicTacToe.State     as State
import qualified TicTacToe.UI        as UI

playTurn :: (Actions m, UI m) => m ()
playTurn = do
  UI.turnScreen
  position <- UI.getPositionInput
  Actions.setCell position
  Actions.switchPlayer

gameLoop :: (Actions m, UI m, Monad m) => m ()
gameLoop = do
  whileM_ gameIsRunning playTurn
  UI.gameOverScreen

gameIsRunning :: Actions m => m Bool
gameIsRunning = (== InPlay) <$> gameState

gameState :: Actions m => m GameState
gameState = GameLogic.getGameState <$> Actions.board

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    evalStateT (runGame gameLoop) State.new
