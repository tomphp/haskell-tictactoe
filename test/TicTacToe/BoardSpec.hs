module TicTacToe.BoardSpec where

import Test.Hspec

import           TicTacToe.Board (Board, Cell(..), Error(..), contains)
import qualified TicTacToe.Board as Board

render :: [Maybe Cell] -> Text
render = fromString . map (\case
                              Just X  -> 'X'
                              Just O  -> 'O'
                              Nothing -> 'E')

boardStr :: Board Cell -> Text
boardStr = Board.render render

spec :: Spec
spec = do
  describe "TicTacToe.Board" $ do
    describe "Board" $ do
      describe "empty" $ do
        it "returns a empty board contains 9 empty cells" $ do
          boardStr Board.empty `shouldBe` replicate 9 'E'

      describe "setCell" $ do
        let cellNum = 5

        let (Right board) = Board.setCell X cellNum Board.empty

        it "sets a cell on the board" $ do
          boardStr board `shouldBe` "EEEEXEEEE"

        it "returns CellDoesNotExist when trying to set an invalid cell" $ do
          Board.setCell X 0 board `shouldBe` Left CellDoesNotExist

        it "returns CellIsNotEmpty when trying to set an invalid cell" $ do
          Board.setCell X cellNum board `shouldBe` Left CellIsNotEmpty

      context "for board in play" $ do
        let board = Board.fromCells [ Just X,  Just O,  Nothing
                                    , Just O,  Just O,  Just X
                                    , Nothing, Nothing, Just X
                                    ]

        describe "lines" $ do
          it "returns lines" $ do
            Board.lines board `shouldBe` [ [ Just X,  Just O,  Nothing ]
                                         , [ Just O,  Just O,  Just X  ]
                                         , [ Nothing, Nothing, Just X  ]
                                         , [ Just X,  Just O,  Nothing ]
                                         , [ Just O,  Just O,  Nothing ]
                                         , [ Nothing, Just X,  Just X  ]
                                         , [ Just X,  Just O,  Just X  ]
                                         , [ Nothing, Just O,  Nothing ]
                                         ]
        describe "row" $ do
          it "returns the row" $ do
            Board.row 1 board `shouldBe` Just [ Just X,   Just O,  Nothing ]
            Board.row 2 board `shouldBe` Just [ Just O,   Just O,  Just X  ]
            Board.row 3 board `shouldBe` Just [ Nothing,  Nothing, Just X  ]

          it "returns Nothing for a bad row number" $ do
            Board.row 0 board `shouldBe` Nothing
            Board.row 4 board `shouldBe` Nothing

        describe "column" $ do
          it "returns the column" $ do
            Board.column 1 board `shouldBe` Just [ Just X,  Just O, Nothing ]
            Board.column 2 board `shouldBe` Just [ Just O,  Just O, Nothing ]
            Board.column 3 board `shouldBe` Just [ Nothing, Just X, Just X  ]

          it "returns Nothing for a bad column number" $ do
            Board.column 0 board `shouldBe` Nothing
            Board.column 4 board `shouldBe` Nothing

        describe "diagonal" $ do
          it "returns the diagonal" $ do
            Board.diagonal 1 board `shouldBe` Just [ Just X,  Just O, Just X ]
            Board.diagonal 2 board `shouldBe` Just [ Nothing, Just O, Nothing ]

          it "returns Nothing for a bad diagonal number" $ do
            Board.diagonal 0 board `shouldBe` Nothing
            Board.diagonal 3 board `shouldBe` Nothing

      describe "render" $ do
        it "converts a board to text" $ do
          let board = Board.fromCells [ Just X, Just O, Nothing
                                      , Just X, Nothing, Just O
                                      , Nothing, Just X, Just O
                                      ]

          let r = fromString . map (\case
                                      Just X  -> 'X'
                                      Just O  -> 'O'
                                      Nothing -> 'E')

          Board.render r board `shouldBe` "XOEXEOEXO"

    describe "contains" $ do
      context "for empty board" $ do
        let b = Board.empty

        it "returns true if board contains expected cell" $ do
          (b `contains` Nothing @(Maybe Cell)) `shouldBe` True
          (b `contains` Just O               ) `shouldBe` False
          (b `contains` Just X               ) `shouldBe` False

      context "for full board" $ do
        let b = Board.fromCells [ Just X, Just O, Just X
                                , Just O, Just X, Just O
                                , Just X, Just O, Just X
                                ]

        it "returns true if board contains expected cell" $ do
          (b `contains` Nothing) `shouldBe` False
          (b `contains` Just O ) `shouldBe` True
          (b `contains` Just X ) `shouldBe` True

    describe "fromCells" $ do
      it "creates a board from a string" $ do
        let cells = [ Just X,  Just O,  Nothing
                    , Just O,  Nothing, Just X
                    , Nothing, Just X,  Just O
                    ]

        boardStr (Board.fromCells cells) `shouldBe` "XOEOEXEXO"
