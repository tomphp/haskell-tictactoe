module TicTacToe.Rendering where

import Data.List (intersperse)

import TicTacToe.Board
import TicTacToe.Player

cellToChar :: (Cell, Int) -> Char
cellToChar cell = case cell of
  (Naught, _)     -> 'O'
  (Cross, _)      -> 'X'
  (Empty, number) -> head $ show number

boardToString :: Board -> String
boardToString board = 
  formatRow (take 3 parsedBoard)
    ++ "\n---------\n"
    ++ formatRow (take 3 $ drop 3 parsedBoard)
    ++ "\n---------\n"
    ++ formatRow (drop 6 parsedBoard)
  where parsedBoard = zipWith (curry cellToChar) board [1..9]
        formatRow row = intersperse ' ' $ intersperse '|' row

playerToString :: Player -> String
playerToString Naughts = "Naughts"
playerToString Crosses = "Crosses"

