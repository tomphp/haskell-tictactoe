module TicTacToe.BoardSpec where

import Control.Error.Safe (atZ)
import Data.List (nub)
import Test.Hspec

import           TicTacToe.Board (BoardError(..), Cell(..))
import qualified TicTacToe.Board as Board

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    describe "Board" $ do
      describe "new" $ do
        it "returns a new board contains 9 empty cells" $ do
          Board.cells Board.new `shouldBe` replicate 9 Empty

      describe "setCell" $ do
        let cellNum = 5
        let cellIndex = pred cellNum

        let (Right board) = Board.setCell Cross cellNum Board.new
        let cells = Board.cells board

        it "sets a cell on the board" $ do
           cells `atZ` cellIndex `shouldBe` Just Cross

        it "doesn't modify other cells" $ do
          nub (take (pred cellIndex) cells) `shouldBe` [Empty]
          nub (drop (succ cellIndex) cells) `shouldBe` [Empty]

        it "returns CellDoesNotExist when trying to set an invalid cell" $ do
          Board.setCell Cross 0 board `shouldBe` Left CellDoesNotExist

        it "returns CellIsNotEmpty when trying to set an invalid cell" $ do
          Board.setCell Cross cellNum board `shouldBe` Left CellIsNotEmpty

      context "for board in play" $ do
        let (Right board) = ( Board.setCell Cross 1  -- row 1
                  >=> Board.setCell Naught 2
                  >=> Board.setCell Empty 3
                  >=> Board.setCell Naught 4 -- row 2
                  >=> Board.setCell Naught 5
                  >=> Board.setCell Cross 6
                  >=> Board.setCell Empty 7 -- row 3
                  >=> Board.setCell Empty 8
                  >=> Board.setCell Cross 9
                  ) Board.new


        describe "lines" $ do
          it "returns lines" $ do
            Board.lines board `shouldBe` [ [ Cross, Naught, Empty ]
                                         , [ Naught, Naught, Cross ]
                                         , [ Empty, Empty, Cross ]
                                         , [ Cross, Naught, Empty ]
                                         , [ Naught, Naught, Empty ]
                                         , [ Empty, Cross, Cross ]
                                         , [ Cross, Naught, Cross ]
                                         , [ Empty, Naught, Empty ]
                                         ]
        describe "row" $ do
          it "returns the row" $ do
            Board.row 1 board `shouldBe` Just [ Cross, Naught, Empty ]
            Board.row 2 board `shouldBe` Just [ Naught, Naught, Cross ]
            Board.row 3 board `shouldBe` Just [ Empty, Empty, Cross ]

          it "returns Nothing for a bad row number" $ do
            Board.row 0 board `shouldBe` Nothing
            Board.row 4 board `shouldBe` Nothing

        describe "column" $ do
          it "returns the column" $ do
            Board.column 1 board `shouldBe` Just [ Cross, Naught, Empty ]
            Board.column 2 board `shouldBe` Just [ Naught, Naught, Empty ]
            Board.column 3 board `shouldBe` Just [ Empty, Cross, Cross ]

          it "returns Nothing for a bad column number" $ do
            Board.column 0 board `shouldBe` Nothing
            Board.column 4 board `shouldBe` Nothing

        describe "diagonal" $ do
          it "returns the diagonal" $ do
            Board.diagonal 1 board `shouldBe` Just [ Cross, Naught, Cross ]
            Board.diagonal 2 board `shouldBe` Just [ Empty, Naught, Empty ]

          it "returns Nothing for a bad diagonal number" $ do
            Board.diagonal 0 board `shouldBe` Nothing
            Board.diagonal 3 board `shouldBe` Nothing

      describe "show" $ do
        it "shows an empty board" $
          show Board.new `shouldBe` concat [ "1 | 2 | 3\n"
                                           , "---------\n"
                                           , "4 | 5 | 6\n"
                                           , "---------\n"
                                           , "7 | 8 | 9"
                                           ]
                      
    describe "BoardError" $ do
      describe "show" $ do
        it "is returns a message from CellDoesNotExist" $
          show CellDoesNotExist `shouldBe` "Attempting to set a cell which does not exist"

        it "is returns a message from CellIsNotEmpty" $
          show CellIsNotEmpty `shouldBe` "Attempting to set a cell which is not empty"
