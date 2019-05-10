{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.TerminalGame (run) where

import Control.Error.Safe (readZ)
import Control.Monad.Loops (untilJust)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, modify)

import           TicTacToe.Game      (Game(..))
import qualified TicTacToe.Game      as Game
import           TicTacToe.Board     (Board, Cell(..))
import qualified TicTacToe.Board     as Board
import           TicTacToe.Player    (Player(..))
import qualified TicTacToe.Player    as Player
import           TicTacToe.State     (TheState)
import qualified TicTacToe.State     as State
import           TicTacToe.UI        (UI)
import qualified TicTacToe.UI        as UI

newtype TerminalGame m a = TerminalGame { runTerminalGame :: StateT TheState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TheState)

run :: Monad m => TerminalGame m a -> TheState -> m a
run game = evalStateT (runTerminalGame game)

instance Monad m => Game (TerminalGame m) where
  player = State.player <$> get

  board = State.board <$> get

  switchPlayer = modify $ State.updatePlayer Player.switch

  setCell position = do
    p <- player
    modify $ State.updateBoard $ Board.setCell (cell p) position

instance MonadIO m => UI (TerminalGame m) where
  gameOverScreen state = do
    b <- Game.board
    drawBoard b
    putStr "TerminalGame over: "
    putStrLn (tshow state)

  turnScreen = do
    b <- Game.board
    p <- player
    drawBoard b
    putStr (tshow p)
    putStr ", "
    putStrLn "choose cell: "

  getPositionInput =
    untilJust $ do
      pos <- readZ . unpack <$> getLine
      when (isNothing pos) $ putStrLn "Try again..."
      return $ pos

cell :: Player -> Cell
cell Naughts = Naught
cell Crosses = Cross

drawBoard :: MonadIO m => Board -> m ()
drawBoard = putStrLn . tshow
