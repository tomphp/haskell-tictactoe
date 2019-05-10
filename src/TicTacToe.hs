module TicTacToe (run) where 

import Control.Monad.Loops (whileM_)

import           TicTacToe.Game         (Game)
import qualified TicTacToe.Game         as Game
import qualified TicTacToe.TerminalGame as Game
import           TicTacToe.UI           (UI)
import qualified TicTacToe.State        as State
import qualified TicTacToe.UI           as UI

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    Game.run gameLoop State.new

gameLoop :: (Game m, UI m, Monad m) => m ()
gameLoop = do
  whileM_ Game.gameIsRunning playTurn

  UI.gameOverScreen =<< Game.state

playTurn :: (Game m, UI m) => m ()
playTurn = do
  UI.turnScreen
  position <- UI.getPositionInput
  Game.setCell position
  Game.switchPlayer

