module TicTacToe.Result (Result(..), fromBoard, isGameOver) where

import           TicTacToe.Board  (Board, Cell(..), contains)
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

data Result = InPlay | Draw | Winner Player deriving (Eq)

instance Show Result where
  show InPlay     = "TerminalGame is in play"
  show Draw       = "Draw"
  show (Winner p) = show p <> " win"

fromBoard :: Board Cell -> Result
fromBoard b = case winnerFromBoard b of
  Just p  -> Winner p
  Nothing -> if b `contains` Nothing then InPlay else Draw

isGameOver :: Result -> Bool
isGameOver InPlay = False
isGameOver _      = True

winnerFromBoard :: Board Cell -> Maybe Player
winnerFromBoard = asum . lineResults

lineResults :: Board Cell -> [Maybe Player]
lineResults = map lineWinner . Board.lines

lineWinner :: [Maybe Cell] -> Maybe Player
lineWinner [Just X, Just X, Just X] = Just Crosses
lineWinner [Just O, Just O, Just O] = Just Naughts
lineWinner _                        = Nothing
