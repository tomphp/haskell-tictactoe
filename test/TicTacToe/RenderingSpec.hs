module TicTacToe.RenderingSpec where

import Test.Hspec

import TicTacToe.Rendering
import TicTacToe.Board
import TicTacToe.Player

spec :: Spec
spec = do
  describe "Rendering" $ do
    context "cellToChar" $ do
      it "renders a naught" $ do
        cellToChar (Naught, 1) `shouldBe` 'O'

      it "renders a cross" $ do
        cellToChar (Cross, 1) `shouldBe` 'X'

      it "renders the cell number for an empty cell" $ do
        cellToChar (Empty, 1) `shouldBe` '1'

    context "boardToString" $ do
      it "converts a board to a string" $ do
        let board = [ Naught, Cross,  Empty
                    , Empty,  Cross,  Naught
                    , Empty,  Naught, Naught
                    ]

        boardToString board `shouldBe` "O | X | 3\n" ++
                                       "---------\n" ++
                                       "4 | X | O\n" ++
                                       "---------\n" ++
                                       "7 | O | O"

    context "playerToString" $ do
      it "converts naughts to string" $ do
        playerToString Naughts `shouldBe` "Naughts"

      it "converts crosses to string" $ do
        playerToString Crosses `shouldBe` "Crosses"
