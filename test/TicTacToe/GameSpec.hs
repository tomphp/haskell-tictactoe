{-# LANGUAGE TemplateHaskell #-}

module TicTacToe.GameSpec where

import Control.Error.Safe   (headZ, tailZ)
import Control.Lens         ((^.), (.=), makeLenses, use)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.RWS    (RWST, runRWST)
import Control.Monad.State  (MonadState(put, get))
import Control.Monad.Writer (MonadWriter(tell))

import Test.Hspec

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import qualified TicTacToe.Game   as Game
import           TicTacToe.Player (Player(Naughts, Crosses))
import qualified TicTacToe.Result as Result
import           TicTacToe.State  (State(..))
import qualified TicTacToe.State  as State
import           TicTacToe.UI     (UI(..))

-- Mock

type Inputs = [Int]

data Output = DisplayBoard (Board Cell) | DisplayMessage Text deriving (Eq, Show)
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
                        let Just h = headZ ins
                        let Just t = tailZ ins
                        putInputs t
                        return h

putInputs :: Monad m => Inputs -> TestGame m ()
putInputs ins = TestGame $ inputs .= ins

getInputs :: Monad m => TestGame m Inputs
getInputs = TestGame $ use inputs

instance Monad m => MonadState State (TestGame m) where
  put s = TestGame $ state .= s
  get = TestGame $ use state

newState :: Board Cell -> Player -> Inputs -> TestState
newState b p ins = TestState { _state = State { _board = b, _player = p }
                             , _inputs = ins
                             }

toCells :: String -> [Maybe Cell]
toCells = map charToCell
  where charToCell 'X' = Just X
        charToCell 'O' = Just O
        charToCell _   = Nothing

-- Tests 

spec :: Spec
spec = do
  describe "TicTacToe.Game" $ do
    describe "game" $ do
      it "something" $ do
        pending

    describe "playTurn" $ do
      before (runTestGame Game.playTurn $ newState Board.empty Crosses [1])
        $ context "new game" $ do
          it "does not finish the game" $ \(Right (result, _, _)) -> do
            Result.isGameOver result `shouldBe` False

          it "displays the board and requests an action" $ \(Right (_, _, outputs)) -> do
            outputs `shouldBe` [ DisplayBoard Board.empty
                               , DisplayMessage "Crosses, choose cell:"
                               ]

          it "switches player" $ \(Right (_, st, _)) -> do
            st^.state^.State.player `shouldBe` Naughts

          it "sets the cell" $ \(Right (_, st, _)) -> do
            st^.state^.State.board `shouldBe` Board.fromCells (toCells "X        ")

      before (runTestGame Game.playTurn $ newState (Board.fromCells (toCells "X        ")) Naughts [1, 2])
        $ context "cross take" $ do
          it "does not finish the game" $ \(Right (result, _, _)) -> do
            Result.isGameOver result `shouldBe` False

          it "displays the board and requests an action" $ \(Right (_, _, outputs)) -> do
            outputs `shouldBe` [ DisplayBoard (Board.fromCells (toCells "X        "))
                               , DisplayMessage "Naughts, choose cell:"
                               , DisplayMessage "Attempting to set a cell which is not empty"
                               , DisplayMessage "Try again"
                               , DisplayBoard (Board.fromCells (toCells "X        "))
                               , DisplayMessage "Naughts, choose cell:"
                               ]

          it "switches player" $ \(Right (_, st, _)) -> do
            st^.state^.State.player `shouldBe` Crosses

          it "sets the cell" $ \(Right (_, st, _)) -> do
            st^.state^.State.board `shouldBe` Board.fromCells (toCells "XO       ")

      before (runTestGame Game.playTurn $ newState Board.empty Crosses [0, 1])
        $ context "invalid cell number entered" $ do
          it "does not finish the game" $ \(Right (result, _, _)) -> do
            Result.isGameOver result `shouldBe` False

          it "displays the board and requests an action" $ \(Right (_, _, outputs)) -> do
            outputs `shouldBe` [ DisplayBoard Board.empty
                               , DisplayMessage "Crosses, choose cell:"
                               , DisplayMessage "Attempting to set a cell which does not exist"
                               , DisplayMessage "Try again"
                               , DisplayBoard Board.empty
                               , DisplayMessage "Crosses, choose cell:"
                               ]

          it "switches player" $ \(Right (_, st, _)) -> do
            st^.state^.State.player `shouldBe` Naughts

          it "sets the cell" $ \(Right (_, st, _)) -> do
            st^.state^.State.board `shouldBe` Board.fromCells (toCells "X        ")

      before (runTestGame Game.playTurn $ newState (Board.fromCells (toCells "XO XO    ")) Crosses [7])
        $ context "crosses win" $ do
          it "does not finish the game" $ \(Right (result, _, _)) -> do
            Result.isGameOver result `shouldBe` True

          it "displays the board and requests an action" $ \(Right (_, _, outputs)) -> do
            outputs `shouldBe` [ DisplayBoard (Board.fromCells $ toCells "XO XO    ")
                               , DisplayMessage "Crosses, choose cell:"
                               ]

          it "sets the cell" $ \(Right (_, st, _)) -> do
            st^.state^.State.board `shouldBe` Board.fromCells (toCells "XO XO X  ")

    describe "gameOverScreen" $ do
      it "something" $ do
        pending
