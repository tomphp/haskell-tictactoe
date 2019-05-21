module TicTacToe.ResultSpec where

import Test.Hspec

import TicTacToe.Result (Result(..))
import TicTacToe.Player (Player(..))

spec :: Spec
spec = do
  describe "TicTacToe.Result" $ do
    describe "Result" $ do
      describe "show" $ do
        it "returns a string" $ do
          show InPlay `shouldBe` "TerminalGame is in play"
          show Draw `shouldBe` "Draw"
          show (Winner Crosses) `shouldBe` "Crosses win"
          show (Winner Naughts) `shouldBe` "Naughts win"

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