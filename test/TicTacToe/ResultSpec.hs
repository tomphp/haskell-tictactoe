module TicTacToe.ResultSpec where

import Test.Hspec

import qualified TicTacToe.Fixtures as Fixtures
import qualified TicTacToe.Result   as Result

spec :: Spec
spec =
  describe "TicTacToe.Result" $ do
    context "for in play board" $ do
      let board = Fixtures.inPlayBoard
      let result = Result.fromBoard board

      describe "show" $
        it "returns a string" $
          show result `shouldBe` "TerminalGame is in play"

      describe "getBoard" $
        it "returns the board" $
          Result.board result `shouldBe` board

    context "for cross won board" $ do
      let board = Fixtures.xWonBoard
      let result = Result.fromBoard board

      describe "show" $
        it "returns a string" $
          show result `shouldBe` "Crosses win"

      describe "getBoard" $
        it "returns the board" $
          Result.board result `shouldBe` board

    context "for naught won board" $ do
      let board = Fixtures.oWonBoard
      let result = Result.fromBoard board

      describe "show" $
        it "returns a string" $
          show result `shouldBe` "Naughts win"

      describe "getBoard" $
        it "returns the board" $
          Result.board result `shouldBe` board

    context "for drawn oard" $ do
      let board = Fixtures.drawnBoard
      let result = Result.fromBoard board

      describe "show" $
        it "returns a string" $
          show result `shouldBe` "Draw"

      describe "getBoard" $
        it "returns the board" $
          Result.board result `shouldBe` board

    -- context "fromBoard" $ do
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
