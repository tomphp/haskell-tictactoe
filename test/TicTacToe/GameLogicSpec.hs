module TicTacToe.GameLogicSpec where

import Data.List (nub)
import Data.Maybe
import Test.Hspec

import           TicTacToe.Board     (Board(..), Cell(..))
import qualified TicTacToe.Board     as Board
import           TicTacToe.GameLogic (GameState(..))
import qualified TicTacToe.GameLogic as GameLogic
import           TicTacToe.Player    (Player(..))

spec :: Spec
spec = do
  describe "TicTacToe.GameLogic" $ do
    describe "GameState" $ do
      describe "show" $ do
        it "returns a string" $ do
        show InPlay `shouldBe` "Game is in play"
        show Draw `shouldBe` "Draw"
        show (Winner Crosses) `shouldBe` "Crosses win"
        show (Winner Naughts) `shouldBe` "Naughts win"

    context "gameLines" $ do
      it "contains all horizontal lines on the board" $ do
        [0, 1, 2] `elem` GameLogic.gameLines `shouldBe` True
        [3, 4, 5] `elem` GameLogic.gameLines `shouldBe` True
        [6, 7, 8] `elem` GameLogic.gameLines `shouldBe` True

      it "contains all vertical lines on the board" $ do
        [0, 3, 6] `elem` GameLogic.gameLines `shouldBe` True
        [1, 4, 7] `elem` GameLogic.gameLines `shouldBe` True
        [2, 5, 8] `elem` GameLogic.gameLines `shouldBe` True

      it "contains all diagonal lines on the board" $ do
        [0, 4, 8] `elem` GameLogic.gameLines `shouldBe` True
        [2, 4, 6] `elem` GameLogic.gameLines `shouldBe` True

    context "cellLine" $ do
      it "gets a line from the board" $ do
        let board = Board [ Naught, Cross,  Empty
                          , Cross,  Naught, Empty
                          , Cross,  Naught, Empty
                          ]
        GameLogic.cellLine board [0, 4, 8] `shouldBe` Just [Naught, Naught, Empty]

    context "getWinner" $ do
      it "is no winner if all cells are empty" $ do
        GameLogic.getWinner (replicate 3 Empty) `shouldBe` Nothing

      it "gives crosses as the winner if line contains all crosses" $ do
        GameLogic.getWinner (replicate 3 Cross) `shouldBe` Just Crosses

      it "gives naughts as the winner if line contains all naughts" $ do
        GameLogic.getWinner (replicate 3 Naught) `shouldBe` Just Naughts

      it "is no winner if all cells are not all the same" $ do
        GameLogic.getWinner [Naught, Cross, Cross] `shouldBe` Nothing

    context "getGameState" $ do
      it "is in play if there are empty cells and no winning lines" $ do
        let board = Board [ Naught, Cross,  Empty
                          , Naught, Cross,  Cross
                          , Cross,  Naught, Naught
                          ]
        GameLogic.getGameState board `shouldBe` InPlay

      it "is a draw if all cells are taken and there are no winning lines" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Naught, Cross,  Cross
                          , Cross,  Naught, Naught
                          ]
        GameLogic.getGameState board `shouldBe` Draw

      it "has been won be crosses if the there is a winning line of crosses" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Cross,  Cross,  Cross
                          , Empty,  Naught, Naught
                          ]
        GameLogic.getGameState board `shouldBe` Winner Crosses
         

      it "has been won be naughts if the there is a winning line of naughts" $ do
        let board = Board [ Naught, Cross,  Naught
                          , Cross,  Naught, Cross
                          , Empty,  Naught, Naught
                          ]
        GameLogic.getGameState board `shouldBe` Winner Naughts
