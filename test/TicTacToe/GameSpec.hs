{-# LANGUAGE TemplateHaskell #-}

module TicTacToe.GameSpec where

import Control.Error.Safe   (headZ, tailZ)
import Control.Lens         ((^.), (.=), makeLenses, use)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.RWS    (RWST, runRWST)
import Control.Monad.State  (MonadState(put, get))
import Control.Monad.Writer (MonadWriter(tell))

import Test.Hspec

import           TicTacToe.Board  (Board)
import qualified TicTacToe.Board  as Board
import qualified TicTacToe.Game   as Game
import           TicTacToe.Player (Player(Naughts))
import qualified TicTacToe.Result as Result
import           TicTacToe.State  (State)
import qualified TicTacToe.State  as State
import           TicTacToe.UI     (UI(..))

-- Mock

type Inputs = [Int]

data Output = DisplayBoard Board | DisplayMessage Text deriving (Eq, Show)
type Outputs = [Output]

data TestState = TestState { _state :: State, _inputs :: Inputs }

makeLenses ''TestState

newtype TestGame m a = TestGame { unApp :: RWST Inputs Outputs TestState (ExceptT Game.Error m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Game.Error
           , MonadWriter Outputs
           )

runTestGame :: Monad m => TestGame m a -> TestState -> m (Either Game.Error (a, TestState, Outputs))
runTestGame app s = runExceptT $ runRWST (unApp app) [] s

instance Monad m => UI (TestGame m) where
  displayMessage msg = tell [DisplayMessage msg]
  displayBoard board = tell [DisplayBoard board]
  getPositionInput = do ins <- getInputs
                        let (Just h) = headZ ins
                        let (Just t) = tailZ ins
                        putInputs t
                        return h

putInputs :: Monad m => Inputs -> TestGame m ()
putInputs ins = TestGame $ inputs .= ins

getInputs :: Monad m => TestGame m Inputs
getInputs = TestGame $ use inputs

instance Monad m => MonadState State (TestGame m) where
  put s = TestGame $ state .= s
  get = TestGame $ use state

-- Tests 

spec :: Spec
spec = do
  describe "TicTacToe.Game" $ do
    describe "game" $ do
      it "something" $ do
        pending

    describe "playTurn" $ do
      before (runTestGame Game.playTurn (TestState State.new [1])) $ context "new game" $ do
        it "does not finish the game" $ \(Right (result, _, _)) -> do
          Result.isGameOver result `shouldBe` False

        it "displays the board and requests an action" $ \(Right (_, _, outputs)) -> do
          outputs `shouldBe` [ DisplayBoard Board.new
                             , DisplayMessage "Crosses, choose cell:"
                             ]

        it "switches player" $ \(Right (_, st, _)) -> do
          st^.state^.State.player `shouldBe` Naughts

        it "sets the cell" $ \(Right (_, st, _)) -> do
          st^.state^.State.board `shouldBe` Board.fromStr "X        "

    describe "gameOverScreen" $ do
      it "something" $ do
        pending

    -- context "getGameState" $ do
    --   it "is in play if there are empty cells and no winning lines" $ do
    --     let board = Board [ Naught, Cross,  Empty
    --                       , Naught, Cross,  Cross
    --                       , Cross,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` InPlay

    --   it "is a draw if all cells are taken and there are no winning lines" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Naught, Cross,  Cross
    --                       , Cross,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Draw

    --   it "has been won be crosses if the there is a winning line of crosses" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Cross,  Cross,  Cross
    --                       , Empty,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Winner Crosses
         

    --   it "has been won be naughts if the there is a winning line of naughts" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Cross,  Naught, Cross
    --                       , Empty,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Winner Naughts
