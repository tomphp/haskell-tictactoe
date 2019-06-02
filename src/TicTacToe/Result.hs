module TicTacToe.Result
  ( Result(..)
  , board
  , fromBoard
  , isGameOver
  ) where

import           TicTacToe.Board (Board, contains)
import qualified TicTacToe.Board as Board
import qualified TicTacToe.Line  as Line

data Result p = InPlay (Board p)
              | Draw (Board p)
              | Winner (Board p) p
              deriving (Eq)

instance Show a => Show (Result a) where
  show (InPlay _)   = "TerminalGame is in play"
  show (Draw _)     = "Draw"
  show (Winner _ p) = show p <> " win"

fromBoard :: Eq a => Board a -> Result a
fromBoard b = case winnerFromBoard b of
  Just p  -> Winner b p
  Nothing -> if b `contains` Nothing then InPlay b else Draw b

board :: Result a -> Board a
board (InPlay b  ) = b
board (Winner b _) = b
board (Draw b)     = b

isGameOver :: Result a -> Bool
isGameOver (InPlay _) = False
isGameOver _          = True

winnerFromBoard :: Eq a => Board a -> Maybe a
winnerFromBoard = asum . lineResults

lineResults :: Eq a => Board a -> [Maybe a]
lineResults = map Line.winner . Board.lines
