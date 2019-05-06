module TicTacToe.BoardSpec where

import Control.Error.Safe (atZ)
import Data.List (nub)
import Test.Hspec
import TicTacToe.Board

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    context "newBoard" $ do
      it "a new board contains 9 cells" $ do
        length newBoard `shouldBe` 9

      it "sets all cells to Empty" $ do
        nub newBoard `shouldBe` [Empty]

    context "setCell" $ do
      let cellNum = 5
      let board = setCell newBoard Cross cellNum 

      it "sets a cell on the board" $ do
        board `atZ` cellNum `shouldBe` Just Cross

      it "doesn't modify other cells" $ do
        (nub $ take (pred cellNum) board) `shouldBe` [Empty]
        (nub $ drop (succ cellNum) board) `shouldBe` [Empty]

      -- it "throws if the cell is already set" $ do
      --   (setCell board Naught cellNum) `shouldThrow` anyErrorCall
