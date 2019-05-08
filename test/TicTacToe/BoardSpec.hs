module TicTacToe.BoardSpec where

import Control.Error.Safe (atZ)
import Data.List (nub)
import Test.Hspec

import           TicTacToe.Board (Cell(..))
import qualified TicTacToe.Board as Board

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    describe "new" $ do
      it "returns a new board contains 9 cells" $ do
        length (Board.cells Board.new) `shouldBe` 9

      it "sets all cells to Empty" $ do
        nub (Board.cells Board.new) `shouldBe` [Empty]

    describe "setCell" $ do
      let cellNum = 5
      let board = Board.setCell Cross cellNum Board.new

      it "sets a cell on the board" $ do
        Board.cells board `atZ` cellNum `shouldBe` Just Cross

      it "doesn't modify other cells" $ do
        nub (take (pred cellNum) (Board.cells board)) `shouldBe` [Empty]
        nub (drop (succ cellNum) (Board.cells board)) `shouldBe` [Empty]

      -- it "throws if the cell is already set" $ do
      --   (setCell board Naught cellNum) `shouldThrow` anyErrorCall
      --
    describe "show" $ do
      it "shows an empty board" $
        show Board.new `shouldBe` concat [ "1 | 2 | 3\n"
                                         , "---------\n"
                                         , "4 | 5 | 6\n"
                                         , "---------\n"
                                         , "7 | 8 | 9"
                                         ]
                    
