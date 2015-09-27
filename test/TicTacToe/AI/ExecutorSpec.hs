module TicTacToe.AI.ExecutorSpec where

import Test.Hspec
import TicTacToe.AI.Executor
import TicTacToe.AI.ParseTree
import TicTacToe.Board
import TicTacToe.Player

spec :: Spec
spec = do
  describe "TicTacToe.AI.Executor" $ do
    let success = 1
    let fail    = 0

    let board = [ Naught, Cross, Empty,
                  Naught, Empty, Cross,
                  Cross,  Empty,  Naught ]

    let emptyCell  = 2
    let takenCell = 0
    let currentCell = 4
    let otherCell = 0
    let myCell = 1
    let theirCell = 0

    let move = (board, Crosses, currentCell)

    let ifAndNode condition cell = IfAndNode (condition cell)
                                             (Result success)
                                             (Result fail)

    context "ParseTree.Result" $ do
      it "returns the the values" $ do
        execute (Result 5) move `shouldBe` 5

    context "ParseTree.CurrentCellIsCond" $ do
      it "confirms the current cell is the one in play" $ do
        let program = ifAndNode CurrentCellIsCond currentCell

        execute program move `shouldBe` success

      it "fails if the current cell is not the one in play" $ do
        let program = ifAndNode CurrentCellIsCond otherCell

        execute program move `shouldBe` fail

    context "ParseTree.CellIsEmptyCond" $ do
      it "confirms the cell is empty" $ do
        let program = ifAndNode CellIsEmptyCond emptyCell

        execute program move `shouldBe` success

      it "fails if the cell is not empty" $ do
        let program = ifAndNode CellIsEmptyCond takenCell

        execute program move `shouldBe` fail

    context "ParseTree.CellIsNotEmptyCond" $ do
      it "confirms the cell is not empty" $ do
        let program = ifAndNode CellIsNotEmptyCond takenCell

        execute program move `shouldBe` success

      it "fails if the cell is empty" $ do
        let program = ifAndNode CellIsNotEmptyCond emptyCell

        execute program move `shouldBe` fail

    context "ParseTree.CellIsMineCond" $ do
      it "confirms the cell is the current player's" $ do
        let program = ifAndNode CellIsMineCond myCell

        execute program move `shouldBe` success

      it "fails if the cell is the other player's" $ do
        let program = ifAndNode CellIsMineCond theirCell

        execute program move `shouldBe` fail

      it "fails if the cell is empty" $ do
        let program = ifAndNode CellIsMineCond emptyCell

        execute program move `shouldBe` fail

    context "ParseTree.CellIsTheirsCond" $ do
      it "confirms the cell is the other player's" $ do
        let program = ifAndNode CellIsTheirsCond theirCell

        execute program move `shouldBe` success

      it "fails if the cell is the current player's" $ do
        let program = ifAndNode CellIsTheirsCond myCell

        execute program move `shouldBe` fail

      it "fails if the cell is empty" $ do
        let program = ifAndNode CellIsTheirsCond emptyCell

        execute program move `shouldBe` fail

