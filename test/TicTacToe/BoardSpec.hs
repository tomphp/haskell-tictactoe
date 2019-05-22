module TicTacToe.BoardSpec where

import Control.Error.Safe (atZ)
import Data.List (nub)
import Test.Hspec

import           TicTacToe.Board (Cell(..), Error(..))
import qualified TicTacToe.Board as Board

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    describe "Board" $ do
      describe "empty" $ do
        it "returns a empty board contains 9 empty cells" $ do
          Board.cells Board.empty `shouldBe` replicate 9 Empty

      describe "setCell" $ do
        let cellNum = 5
        let cellIndex = pred cellNum

        let (Right board) = Board.setCell X cellNum Board.empty
        let cells = Board.cells board

        it "sets a cell on the board" $ do
           cells `atZ` cellIndex `shouldBe` Just X

        it "doesn't modify other cells" $ do
          nub (take (pred cellIndex) cells) `shouldBe` [Empty]
          nub (drop (succ cellIndex) cells) `shouldBe` [Empty]

        it "returns CellDoesNotExist when trying to set an invalid cell" $ do
          Board.setCell X 0 board `shouldBe` Left CellDoesNotExist

        it "returns CellIsNotEmpty when trying to set an invalid cell" $ do
          Board.setCell X cellNum board `shouldBe` Left CellIsNotEmpty

      context "for board in play" $ do
        let (Right board) = ( Board.setCell X 1  -- row 1
                  >=> Board.setCell O 2
                  >=> Board.setCell Empty 3
                  >=> Board.setCell O 4 -- row 2
                  >=> Board.setCell O 5
                  >=> Board.setCell X 6
                  >=> Board.setCell Empty 7 -- row 3
                  >=> Board.setCell Empty 8
                  >=> Board.setCell X 9
                  ) Board.empty


        describe "lines" $ do
          it "returns lines" $ do
            Board.lines board `shouldBe` [ [ X,     O,     Empty ]
                                         , [ O,     O,     X     ]
                                         , [ Empty, Empty, X     ]
                                         , [ X,     O,     Empty ]
                                         , [ O,     O,     Empty ]
                                         , [ Empty, X,     X     ]
                                         , [ X,     O,     X     ]
                                         , [ Empty, O,     Empty ]
                                         ]
        describe "row" $ do
          it "returns the row" $ do
            Board.row 1 board `shouldBe` Just [ X,     O,     Empty ]
            Board.row 2 board `shouldBe` Just [ O,     O,     X     ]
            Board.row 3 board `shouldBe` Just [ Empty, Empty, X     ]

          it "returns Nothing for a bad row number" $ do
            Board.row 0 board `shouldBe` Nothing
            Board.row 4 board `shouldBe` Nothing

        describe "column" $ do
          it "returns the column" $ do
            Board.column 1 board `shouldBe` Just [ X,     O, Empty ]
            Board.column 2 board `shouldBe` Just [ O,     O, Empty ]
            Board.column 3 board `shouldBe` Just [ Empty, X, X     ]

          it "returns Nothing for a bad column number" $ do
            Board.column 0 board `shouldBe` Nothing
            Board.column 4 board `shouldBe` Nothing

        describe "diagonal" $ do
          it "returns the diagonal" $ do
            Board.diagonal 1 board `shouldBe` Just [ X,     O, X     ]
            Board.diagonal 2 board `shouldBe` Just [ Empty, O, Empty ]

          it "returns Nothing for a bad diagonal number" $ do
            Board.diagonal 0 board `shouldBe` Nothing
            Board.diagonal 3 board `shouldBe` Nothing

      describe "render" $ do
        it "converts a board to text" $ do
          let board = Board.fromCells [ X, O, Empty, X, Empty, O, Empty, X, O ]

          let r = fromString . map (\case
                                      X     -> 'X'
                                      O     -> 'O'
                                      Empty -> 'E')

          Board.render r board `shouldBe` "XOEXEOEXO"

    describe "fromCells" $ do
      it "creates a board from a string" $ do
        let cells = [ X,     O,     Empty
                    , O,     Empty, X
                    , Empty, X,     O
                    ]

        Board.cells (Board.fromCells cells) `shouldBe` cells
