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

    context "ParseTree.Result" $ do
      it "returns the the values" $ do
        execute (Result 5) move `shouldBe` 5

    context "ParseTree.CurrentCellIsCond" $ do
      it "confirms the current cell is the one in play" $ do
        let program = IfAndNode (CurrentCellIsCond currentCell) (Result success) (Result fail)

        execute program move `shouldBe` success

      it "fails if the current cell is not the one in play" $ do
        let program = IfAndNode (CurrentCellIsCond otherCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

    context "ParseTree.CellIsEmptyCond" $ do
      it "confirms the cell is empty" $ do
        let program = IfAndNode (CellIsEmptyCond emptyCell) (Result success) (Result fail)

        execute program move `shouldBe` success

      it "fails if the cell is not empty" $ do
        let program = IfAndNode (CellIsEmptyCond takenCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

    context "ParseTree.CellIsNotEmptyCond" $ do
      it "confirms the cell is not empty" $ do
        let program = IfAndNode (CellIsNotEmptyCond takenCell) (Result success) (Result fail)

        execute program move `shouldBe` success

      it "fails if the cell is empty" $ do
        let program = IfAndNode (CellIsNotEmptyCond emptyCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

    context "ParseTree.CellIsMineCond" $ do
      it "confirms the cell is the current player's" $ do
        let program = IfAndNode (CellIsMineCond myCell) (Result success) (Result fail)

        execute program move `shouldBe` success

      it "fails if the cell is the other player's" $ do
        let program = IfAndNode (CellIsMineCond theirCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

      it "fails if the cell is empty" $ do
        let program = IfAndNode (CellIsMineCond emptyCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

    context "ParseTree.CellIsTheirsCond" $ do
      it "confirms the cell is the other player's" $ do
        let program = IfAndNode (CellIsTheirsCond theirCell) (Result success) (Result fail)

        execute program move `shouldBe` success

      it "fails if the cell is the current player's" $ do
        let program = IfAndNode (CellIsTheirsCond myCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

      it "fails if the cell is empty" $ do
        let program = IfAndNode (CellIsTheirsCond emptyCell) (Result success) (Result fail)

        execute program move `shouldBe` fail

