module TicTacToe.Result (Result(..), fromBoard, isGameOver) where

import           TicTacToe.Board (Board, contains)
import qualified TicTacToe.Board as Board
import qualified TicTacToe.Line  as Line

data Result p = InPlay
              | Draw
              | Winner p
              deriving (Eq)

instance Show a => Show (Result a) where
  show InPlay     = "TerminalGame is in play"
  show Draw       = "Draw"
  show (Winner p) = show p <> " win"

fromBoard :: Eq a => Board a -> Result a
fromBoard b = case winnerFromBoard b of
  Just p  -> Winner p
  Nothing -> if b `contains` Nothing then InPlay else Draw

isGameOver :: Result a -> Bool
isGameOver InPlay = False
isGameOver _      = True

winnerFromBoard :: Eq a => Board a -> Maybe a
winnerFromBoard = asum . lineResults

lineResults :: Eq a => Board a -> [Maybe a]
lineResults = map Line.winner . Board.lines
