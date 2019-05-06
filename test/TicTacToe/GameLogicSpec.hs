module TicTacToe.GameLogicSpec where

import Data.List (nub)
import Test.Hspec
import TicTacToe.Board
import TicTacToe.GameLogic
import TicTacToe.Player
import Data.Maybe

spec :: Spec
spec = do
  describe "TicTacToe.GameLogic" $ do
    context "gameLines" $ do
      it "contains all horizontal lines on the board" $ do
        [0, 1, 2] `elem` gameLines `shouldBe` True
        [3, 4, 5] `elem` gameLines `shouldBe` True
        [6, 7, 8] `elem` gameLines `shouldBe` True

      it "contains all vertical lines on the board" $ do
        [0, 3, 6] `elem` gameLines `shouldBe` True
        [1, 4, 7] `elem` gameLines `shouldBe` True
        [2, 5, 8] `elem` gameLines `shouldBe` True

      it "contains all diagonal lines on the board" $ do
        [0, 4, 8] `elem` gameLines `shouldBe` True
        [2, 4, 6] `elem` gameLines `shouldBe` True

    context "cellLine" $ do
      it "gets a line from the board" $ do
        let board = Board [ Naught, Cross,  Empty
                          , Cross,  Naught, Empty
                          , Cross,  Naught, Empty
                          ]
        cellLine board [0, 4, 8] `shouldBe` Just [Naught, Naught, Empty]

    context "getWinner" $ do
      it "is no winner if all cells are empty" $ do
        (getWinner $ replicate 3 Empty) `shouldBe` Nothing

      it "gives crosses as the winner if line contains all crosses" $ do
        (getWinner $ replicate 3 Cross) `shouldBe` Just Crosses

      it "gives naughts as the winner if line contains all naughts" $ do
        (getWinner $ replicate 3 Naught) `shouldBe` Just Naughts

      it "is no winner if all cells are not all the same" $ do
        getWinner [Naught, Cross, Cross] `shouldBe` Nothing

    context "getGameState" $ do
      it "is in play if there are empty cells and no winning lines" $ do
        let board = Board [ Naught, Cross,  Empty
                          , Naught, Cross,  Cross
                          , Cross,  Naught, Naught
                          ]
        getGameState board `shouldBe` InPlay

      it "is a draw if all cells are taken and there are no winning lines" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Naught, Cross,  Cross
                          , Cross,  Naught, Naught
                          ]
        getGameState board `shouldBe` Draw

      it "has been won be crosses if the there is a winning line of crosses" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Cross,  Cross,  Cross
                          , Empty,  Naught, Naught
                          ]
        getGameState board `shouldBe` Winner Crosses
         

      it "has been won be naughts if the there is a winning line of naughts" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Cross,  Naught, Cross
                          , Empty,  Naught, Naught
                          ]
        getGameState board `shouldBe` Winner Naughts
