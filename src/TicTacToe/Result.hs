module TicTacToe.Result (Result(..), fromBoard, isGameOver) where

import           TicTacToe.Board  (Board, contains)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

data Result = InPlay | Draw | Winner Player deriving (Eq)

instance Show Result where
  show InPlay     = "TerminalGame is in play"
  show Draw       = "Draw"
  show (Winner p) = show p <> " win"

fromBoard :: Board Player -> Result
fromBoard b = case winnerFromBoard b of
  Just p  -> Winner p
  Nothing -> if b `contains` Nothing then InPlay else Draw

isGameOver :: Result -> Bool
isGameOver InPlay = False
isGameOver _      = True

winnerFromBoard :: Board Player -> Maybe Player
winnerFromBoard = asum . lineResults

lineResults :: Board Player -> [Maybe Player]
lineResults = map lineWinner . Board.lines

lineWinner :: [Maybe Player] -> Maybe Player
lineWinner [Just X, Just X, Just X] = Just X
lineWinner [Just O, Just O, Just O] = Just O
lineWinner _                        = Nothing
