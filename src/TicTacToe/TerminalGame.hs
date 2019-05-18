module TicTacToe.TerminalGame (run) where

import Control.Error.Safe         (readZ)
import Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import Control.Monad.Loops        (untilJust)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)

import           TicTacToe.Game   (State)
import qualified TicTacToe.Game   as Game
import qualified TicTacToe.State  as State
import           TicTacToe.UI     (UI(..))

newtype TerminalGame m a = TerminalGame { runTerminalGame :: StateT State (ExceptT Game.Error m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState State
           , MonadError Game.Error
           )

run :: Monad m => TerminalGame m a -> m a
run game =
  result >>= \case
                Right v -> return v
                Left _  -> error "Error happened"
  where result = runExceptT $ evalStateT (runTerminalGame game) State.new

instance MonadIO m => UI (TerminalGame m) where
  displayMessage = putStrLn

  displayBoard = putStrLn . tshow

  getPositionInput =
    untilJust $ do
      pos <- readZ . unpack <$> getLine
      when (isNothing pos) $ displayMessage "Try again..."
      return pos
