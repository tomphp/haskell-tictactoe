{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.TerminalGame (run) where

import Control.Error.Safe         (readZ)
import Control.Monad.Loops        (untilJust)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, modify)

import           TicTacToe.Game   (State(..), UI(..))
import           TicTacToe.Board  (Board)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

newtype TerminalGame m a = TerminalGame { runTerminalGame :: StateT TheState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TheState)

data TheState = TheState { theBoard :: Board
                         , thePlayer :: Player
                         }

run :: Monad m => TerminalGame m a -> m a
run game = evalStateT (runTerminalGame game) newState

newState :: TheState
newState = TheState { theBoard = Board.new, thePlayer = Crosses }

instance Monad m => State (TerminalGame m) where
  player = thePlayer <$> get

  board = theBoard <$> get

  updatePlayer = modify . updatePlayer'

  updateBoard = modify . updateBoard'

updatePlayer' :: (Player -> Player) -> TheState -> TheState
updatePlayer' update s@TheState{thePlayer=p} = s { thePlayer = update p }

updateBoard' :: (Board -> Board) -> TheState -> TheState
updateBoard' update s@TheState{theBoard=b} = s { theBoard = update b }

instance MonadIO m => UI (TerminalGame m) where
  displayMessage = putStrLn

  displayBoard = putStrLn . tshow

  getPositionInput =
    untilJust $ do
      pos <- readZ . unpack <$> getLine
      when (isNothing pos) $ displayMessage "Try again..."
      return pos
