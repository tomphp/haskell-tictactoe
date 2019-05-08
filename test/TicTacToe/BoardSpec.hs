module TicTacToe.BoardSpec where

import Control.Error.Safe (atZ)
import Data.List (nub)
import Test.Hspec

import TicTacToe.Board

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    describe "new" $ do
      it "a new board contains 9 cells" $ do
        length (cells new) `shouldBe` 9

      it "sets all cells to Empty" $ do
        nub (cells new) `shouldBe` [Empty]

    describe "setCell" $ do
      let cellNum = 5
      let board = setCell Cross cellNum new

      it "sets a cell on the board" $ do
        cells board `atZ` cellNum `shouldBe` Just Cross

      it "doesn't modify other cells" $ do
        nub (take (pred cellNum) (cells board)) `shouldBe` [Empty]
        nub (drop (succ cellNum) (cells board)) `shouldBe` [Empty]

      -- it "throws if the cell is already set" $ do
      --   (setCell board Naught cellNum) `shouldThrow` anyErrorCall
    
    describe "show" $ do
      it "shows an empty board" $
        show new `shouldBe` concat [ "1 | 2 | 3\n"
                                        , "---------\n"
                                        , "4 | 5 | 6\n"
                                        , "---------\n"
                                        , "7 | 8 | 9"
                                        ]
                    
