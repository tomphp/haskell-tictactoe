module TicTacToe.AISpec where

import System.Random
import Data.List (nub)
import Test.Hspec
import TicTacToe.AI
import TicTacToe.Board

spec :: Spec
spec = do
  describe "TicTacToe.AI" $ do
    context "parse" $ do
      it "returns nothing if there are no valid instructions" $ do
        parse [CellIsEmpty 1] `shouldBe` Nothing

      it "returns nothing if the top level if is incomplete" $ do
        pending
        parse [IfAnd, CellIsEmpty 1] `shouldBe` Nothing

      context "Rank" $ do
        it "returns a constant rank function" $ do
          parse [Rank 5] `shouldBe` (Just $ Result 5)

      context "ifs" $ do
        let cellNumber = 5
        let trueValue = 50
        let falseValue = 100

        context "CurrentCellIs" $ do
          let dna = [IfAnd, CurrentCellIs cellNumber, Rank trueValue, Rank falseValue]

          it "produces a currentCellIs function" $ do
            parse dna `shouldBe` (Just $ IfAndNode (CurrentCellIsCond cellNumber)
                                                   (Result trueValue)
                                                   (Result falseValue))

        context "CellIsEmpty" $ do
          let dna = [IfAnd, CellIsEmpty cellNumber, Rank trueValue, Rank falseValue]

          it "matches the current cell" $ do
            parse dna `shouldBe` (Just $ IfAndNode (CellIsEmptyCond cellNumber)
                                                   (Result trueValue)
                                                   (Result falseValue))

        context "CellIsNotEmpty" $ do
          let dna = [IfAnd, CellIsNotEmpty cellNumber, Rank trueValue, Rank falseValue]

          it "matches the current cell" $ do
            parse dna `shouldBe` (Just $ IfAndNode (CellIsNotEmptyCond cellNumber)
                                                   (Result trueValue)
                                                   (Result falseValue))

        context "CellIsMine" $ do
          let dna = [IfAnd, CellIsMine cellNumber, Rank trueValue, Rank falseValue]

          it "matches the current cell" $ do
            parse dna `shouldBe` (Just $ IfAndNode (CellIsMineCond cellNumber)
                                                   (Result trueValue)
                                                   (Result falseValue))

        context "CellIsTheirs" $ do
          let dna = [IfAnd, CellIsTheirs cellNumber, Rank trueValue, Rank falseValue]

          it "matches the current cell" $ do
            parse dna `shouldBe` (Just $ IfAndNode (CellIsTheirsCond cellNumber)
                                                   (Result trueValue)
                                                   (Result falseValue))

        it "ignores un-executable leading genes" $ do
          let rubbish = [CellIsEmpty 2, CellIsNotEmpty 3]
          let dna = rubbish ++ [IfAnd, CurrentCellIs cellNumber, Rank trueValue, Rank falseValue]

          parse dna `shouldBe` (Just $ IfAndNode (CurrentCellIsCond cellNumber)
                                                 (Result trueValue)
                                                 (Result falseValue))

        it "ignores un-executable genes" $ do
          let rubbish = [CellIsEmpty 2, CellIsNotEmpty 3]
          let dna = rubbish ++ [IfAnd, Rank 2, CurrentCellIs cellNumber, Rank trueValue, CellIsEmpty 1, Rank falseValue]

          parse dna `shouldBe` (Just $ IfAndNode (CurrentCellIsCond cellNumber)
                                                 (Result trueValue)
                                                 (Result falseValue))

