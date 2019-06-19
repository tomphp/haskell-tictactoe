module TicTacToe.GameSpec where

import Control.Lens       ((^.))
import Control.Lens.Tuple (_3)

import Test.Hspec

import           TicTacToe.Board    (Board)
import qualified TicTacToe.Board    as Board
import qualified TicTacToe.Fixtures as Fixtures
import           TicTacToe.TestGame ( Inputs
                                    , Output(..)
                                    , Outputs
                                    , TestState
                                    , newState
                                    , runTestGame
                                    , state
                                    )
import qualified TicTacToe.Game     as Game
import           TicTacToe.Player   (Player(..))
import           TicTacToe.Result   (Result)
import qualified TicTacToe.Result   as Result
import           TicTacToe.State    (board, player)

boardFromString :: String -> Board Player
boardFromString = Fixtures.boardFromString

runGame :: Monad m => Inputs -> m (Either Game.Error Outputs)
runGame inputs = fmap (^._3) <$> returnValue
  where returnValue = runTestGame Game.game emptyState
        emptyState = newState Board.empty X inputs

runPlayTurn :: Monad m
            => String
            -> Player
            -> Inputs
            -> m (Either Game.Error (Result Player, TestState, Outputs))
runPlayTurn b p i = runTestGame Game.playTurn $ newState (boardFromString b) p i

runGameOverScreen :: Monad m => Result Player -> m (Either Game.Error Outputs)
runGameOverScreen result = fmap (^._3) <$> returnValue
  where returnValue = runTestGame (Game.gameOverScreen result) emptyState
        emptyState = newState Board.empty X []

spec :: Spec
spec = describe "TicTacToe.Game" $ do
  describe "game" $
    before (runGame [1, 2, 3, 4, 5, 6, 7])
      $ context "for a complete game" $
        it "displays the game" $ \(Right outputs) -> 
          outputs `shouldBe` [ DisplayBoard Board.empty
                             , DisplayMessage "Crosses, choose cell:"
                             , DisplayBoard $ boardFromString "X        "
                             , DisplayMessage "Naughts, choose cell:"
                             , DisplayBoard $ boardFromString "XO       "
                             , DisplayMessage "Crosses, choose cell:"
                             , DisplayBoard $ boardFromString "XOX      "
                             , DisplayMessage "Naughts, choose cell:"
                             , DisplayBoard $ boardFromString "XOXO     "
                             , DisplayMessage "Crosses, choose cell:"
                             , DisplayBoard $ boardFromString "XOXOX    "
                             , DisplayMessage "Naughts, choose cell:"
                             , DisplayBoard $ boardFromString "XOXOXO   "
                             , DisplayMessage "Crosses, choose cell:"
                             , DisplayBoard $ boardFromString "XOXOXOX  "
                             , DisplayMessage "Game over: Crosses win"
                             ]

  describe "playTurn" $ do
    before (runPlayTurn "         " X [1])
      $ context "new game" $ do
        it "does not finish the game" $ \(Right (result, _, _)) ->
          Result.isGameOver result `shouldBe` False

        it "displays the board and requests an action" $ \(Right (_, _, outputs)) ->
          outputs `shouldBe` [ DisplayBoard Board.empty
                             , DisplayMessage "Crosses, choose cell:"
                             ]

        it "switches player" $ \(Right (_, st, _)) ->
          st^.state^.player `shouldBe` O

        it "sets the cell" $ \(Right (_, st, _)) ->
          st^.state^.board `shouldBe` boardFromString "X        "

    before (runPlayTurn "X        " O [1, 2])
      $ context "cross take" $ do
        it "does not finish the game" $ \(Right (result, _, _)) ->
          Result.isGameOver result `shouldBe` False

        it "displays the board and requests an action" $ \(Right (_, _, outputs)) ->
          outputs `shouldBe` [ DisplayBoard (boardFromString "X        ")
                             , DisplayMessage "Naughts, choose cell:"
                             , DisplayMessage "Attempting to set a cell which is not empty"
                             , DisplayMessage "Try again"
                             , DisplayBoard (boardFromString "X        ")
                             , DisplayMessage "Naughts, choose cell:"
                             ]

        it "switches player" $ \(Right (_, st, _)) ->
          st^.state^.player `shouldBe` X

        it "sets the cell" $ \(Right (_, st, _)) ->
          st^.state^.board `shouldBe` boardFromString "XO       "

    before (runPlayTurn "XO XO    " X [7])
      $ context "crosses win" $ do
        it "does not finish the game" $ \(Right (result, _, _)) ->
          Result.isGameOver result `shouldBe` True

        it "displays the board and requests an action" $ \(Right (_, _, outputs)) ->
          outputs `shouldBe` [ DisplayBoard (boardFromString "XO XO    ")
                             , DisplayMessage "Crosses, choose cell:"
                             ]

        it "sets the cell" $ \(Right (_, st, _)) ->
          st^.state^.board `shouldBe` boardFromString "XO XO X  "

  describe "gameOverScreen" $ do
    before (runGameOverScreen $ Result.fromBoard Fixtures.inPlayBoard)
      $ context "for in play result" $
        it "displays the result" $ \(Right outputs) ->
          outputs `shouldBe` [ DisplayBoard Fixtures.inPlayBoard
                             , DisplayMessage "Game over: TerminalGame is in play" -- should be an error
                             ]

    before (runGameOverScreen $ Result.fromBoard Fixtures.drawnBoard)
      $ context "for a draw result" $
        it "displays the result" $ \(Right outputs) ->
          outputs `shouldBe` [ DisplayBoard Fixtures.drawnBoard
                             , DisplayMessage "Game over: Draw"
                             ]

    before (runGameOverScreen $ Result.fromBoard Fixtures.xWonBoard)
      $ context "for crosses won result" $
        it "displays the result" $ \(Right outputs) ->
          outputs `shouldBe` [ DisplayBoard Fixtures.xWonBoard
                             , DisplayMessage "Game over: Crosses win"
                             ]

    before (runGameOverScreen $ Result.fromBoard Fixtures.oWonBoard)
      $ context "for crosses won result" $
        it "displays the result" $ \(Right outputs) ->
          outputs `shouldBe` [ DisplayBoard Fixtures.oWonBoard
                             , DisplayMessage "Game over: Naughts win"
                             ]

