{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module TicTacToe (run) where 

import Control.Error.Safe (headZ, readZ)
import Control.Monad.Loops (untilJust, whileM_)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, put, modify)

import           TicTacToe.Board (Board, Cell(..))
import qualified TicTacToe.Board as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player
import           TicTacToe.GameLogic (GameState(..))
import qualified TicTacToe.GameLogic as GameLogic

data TheState = TheState { theBoard :: Board
                         , currentPlayer :: Player
                         }

newState :: TheState
newState = TheState { theBoard = Board.new, currentPlayer = Crosses }

newtype Game m a = Game { runGame :: StateT TheState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TheState)

class Monad m => Actions m where
  player       :: m Player
  board        :: m Board
  switchPlayer :: m ()
  setCell      :: Int -> m ()

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: m ()
  getPositionInput :: m Int

cell :: Player -> Cell
cell Naughts = Naught
cell Crosses = Cross

instance Monad m => Actions (Game m) where
  player = currentPlayer <$> get

  board = theBoard <$> get

  switchPlayer = modify (\s -> s { currentPlayer = Player.switch (currentPlayer s) })

  setCell position = do
    p <- player
    modify (\s -> s { theBoard = Board.setCell (theBoard s) (cell p) position })

instance MonadIO m => UI (Game m) where
  gameOverScreen = do
    b <- board
    let state = GameLogic.getGameState b
    drawBoard b
    putStr "Game over: "
    putStrLn (tshow state)

  turnScreen = do
    b <- board
    p <- player
    drawBoard b
    putStr (tshow p)
    putStr ", "
    putStrLn "choose cell: "

  getPositionInput =
    untilJust $ do
      pos <- readZ . unpack <$> getLine
      when (isNothing pos) $ putStrLn "Try again..."
      return $ pred <$> pos

drawBoard :: MonadIO m => Board -> m ()
drawBoard = putStrLn . tshow

playTurn :: (Actions m, UI m) => m ()
playTurn = do
  turnScreen
  position <- getPositionInput
  setCell position
  switchPlayer

gameLoop :: (Actions m, UI m, Monad m) => m ()
gameLoop = do
  whileM_ gameIsRunning playTurn
  gameOverScreen

gameIsRunning :: Actions m => m Bool
gameIsRunning = (== InPlay) <$> gameState

gameState :: Actions m => m GameState
gameState = GameLogic.getGameState <$> board

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    evalStateT (runGame gameLoop) newState
