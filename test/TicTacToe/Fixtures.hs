module TicTacToe.Fixtures where

import AppPrelude

import           TicTacToe.Board  (Board)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

inPlayBoard :: Board Player
inPlayBoard = Board.fromCells [ Just X,  Just O,  Nothing
                              , Nothing, Nothing, Nothing
                              , Nothing, Nothing, Nothing
                              ]

xWonBoard :: Board Player
xWonBoard = Board.fromCells [ Just X,  Just X,  Just X
                            , Just O,  Nothing, Just O
                            , Nothing, Just O,  Nothing
                            ]

oWonBoard :: Board Player
oWonBoard = Board.fromCells [ Just X,  Just O, Just X
                            , Just X,  Just O, Just X
                            , Nothing, Just O, Nothing
                            ]

drawnBoard :: Board Player
drawnBoard = Board.fromCells [ Just X, Just O, Just X
                             , Just X, Just O, Just X
                             , Just O, Just X, Just O
                             ]

boardFromString :: String -> Board Player
boardFromString = Board.fromCells . toCells
  where toCells = map charToCell

        charToCell 'X' = Just X
        charToCell 'O' = Just O
        charToCell _   = Nothing
