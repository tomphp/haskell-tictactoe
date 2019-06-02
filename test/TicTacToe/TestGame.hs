{-# LANGUAGE TemplateHaskell #-}

module TicTacToe.TestGame
  ( Inputs
  , Output(..)
  , Outputs
  , TestState
  , newState
  , runTestGame
  , state
  ) where

import Control.Lens         (makeLenses, use, (.=))
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.RWS    (RWST, runRWST)
import Control.Monad.State  (MonadState(get, put))
import Control.Monad.Writer (MonadWriter(tell))

import           TicTacToe.Board    (Board)
import qualified TicTacToe.Game     as Game
import           TicTacToe.Player   (Player(..))
import           TicTacToe.State    (State(..))
import           TicTacToe.UI       (UI(..))

type Inputs = [Int]

data Output = DisplayBoard (Board Player)
            | DisplayMessage Text
            deriving (Eq, Show)

type Outputs = [Output]

data TestState = TestState { _state :: State, _inputs :: Inputs }

makeLenses ''TestState

newtype TestGame m a =
  TestGame { unApp :: RWST Inputs Outputs TestState (ExceptT Game.Error m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Game.Error
           , MonadWriter Outputs
           )

runTestGame :: Monad m
            => TestGame m a
            -> TestState
            -> m (Either Game.Error (a, TestState, Outputs))
runTestGame app s = runExceptT $ runRWST (unApp app) [] s

instance Monad m => UI (TestGame m) where
  displayMessage msg = tell [DisplayMessage msg]
  displayBoard b = tell [DisplayBoard b]
  getPositionInput = do ins <- getInputs
                        let (h:t) = ins
                        putInputs t
                        return h

putInputs :: Monad m => Inputs -> TestGame m ()
putInputs ins = TestGame $ inputs .= ins

getInputs :: Monad m => TestGame m Inputs
getInputs = TestGame $ use inputs

instance Monad m => MonadState State (TestGame m) where
  put s = TestGame $ state .= s
  get = TestGame $ use state

newState :: Board Player -> Player -> Inputs -> TestState
newState b p ins = TestState { _state = State { _board = b, _player = p }
                             , _inputs = ins
                             }
