{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.Game where

import Control.Error.Safe (readZ)
import Control.Monad.Loops (untilJust)
import Control.Monad.State.Strict (MonadState, StateT, get, put, modify)

import           TicTacToe.Actions   (Actions(..))
import qualified TicTacToe.Actions   as Actions
import           TicTacToe.Board     (Board, Cell(..))
import qualified TicTacToe.Board     as Board
import qualified TicTacToe.GameLogic as GameLogic
import           TicTacToe.Player    (Player(..))
import qualified TicTacToe.Player    as Player
import           TicTacToe.State     (TheState)
import qualified TicTacToe.State     as State
import           TicTacToe.UI        (UI)
import qualified TicTacToe.UI        as UI

newtype Game m a = Game { runGame :: StateT TheState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TheState)

instance Monad m => Actions (Game m) where
  player = State.currentPlayer <$> get

  board = State.theBoard <$> get

  switchPlayer = modify (\s -> s { State.currentPlayer = Player.switch (State.currentPlayer s) })

  setCell position = do
    p <- player
    modify (\s -> s { State.theBoard = Board.setCell (State.theBoard s) (cell p) position })

instance MonadIO m => UI (Game m) where
  gameOverScreen = do
    b <- Actions.board
    let state = GameLogic.getGameState b
    drawBoard b
    putStr "Game over: "
    putStrLn (tshow state)

  turnScreen = do
    b <- Actions.board
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

cell :: Player -> Cell
cell Naughts = Naught
cell Crosses = Cross

drawBoard :: MonadIO m => Board -> m ()
drawBoard = putStrLn . tshow
