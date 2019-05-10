module TicTacToe (run) where 

import Control.Monad.Loops (whileM_)

import           TicTacToe.Actions   (Actions)
import qualified TicTacToe.Actions   as Actions
import qualified TicTacToe.Game      as Game
import           TicTacToe.UI        (UI)
import qualified TicTacToe.State     as State
import qualified TicTacToe.UI        as UI

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    Game.run gameLoop State.new

gameLoop :: (Actions m, UI m, Monad m) => m ()
gameLoop = do
  whileM_ Actions.gameIsRunning playTurn

  UI.gameOverScreen =<< Actions.state

playTurn :: (Actions m, UI m) => m ()
playTurn = do
  UI.turnScreen
  position <- UI.getPositionInput
  Actions.setCell position
  Actions.switchPlayer

