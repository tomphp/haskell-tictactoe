module TicTacToe.BoardSpec where

import Test.Hspec

import           TicTacToe.Board  (Board, Error(..), contains)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Line   (Line(..))
import           TicTacToe.Player (Player(..))

render :: [Maybe Player] -> Text
render = fromString . map (\case
                              Just X  -> 'X'
                              Just O  -> 'O'
                              Nothing -> 'E')

boardStr :: Board Player -> Text
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
            Board.lines board `shouldBe` [ Line [ Just X,  Just O,  Nothing ]
                                         , Line [ Just O,  Just O,  Just X  ]
                                         , Line [ Nothing, Nothing, Just X  ]
                                         , Line [ Just X,  Just O,  Nothing ]
                                         , Line [ Just O,  Just O,  Nothing ]
                                         , Line [ Nothing, Just X,  Just X  ]
                                         , Line [ Just X,  Just O,  Just X  ]
                                         , Line [ Nothing, Just O,  Nothing ]
                                         ]

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

          Board.render r board `shouldBe` ("XOEXEOEXO" :: Text)

    describe "contains" $ do
      context "for empty board" $ do
        let b = Board.empty

        it "returns true if board contains expected cell" $ do
          (b `contains` Nothing @(Maybe Player)) `shouldBe` True
          (b `contains` Just O                 ) `shouldBe` False
          (b `contains` Just X                 ) `shouldBe` False

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
