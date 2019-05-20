module TicTacToe.Result (Result(..), fromBoard, isGameOver) where

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

data Result = InPlay | Draw | Winner Player deriving (Eq)

instance Show Result where
  show InPlay     = "TerminalGame is in play"
  show Draw       = "Draw"
  show (Winner p) = show p <> " win"

fromBoard :: Board -> Result
fromBoard b = case winnerFromBoard b of
  Just p  -> Winner p
  Nothing -> if Empty `elem` Board.cells b then InPlay else Draw

isGameOver :: Result -> Bool
isGameOver InPlay = False
isGameOver _      = True

winnerFromBoard :: Board -> Maybe Player
winnerFromBoard = asum . lineResults

lineResults :: Board -> [Maybe Player]
lineResults = map lineWinner . Board.lines

lineWinner :: [Cell] -> Maybe Player
lineWinner [X, X, X] = Just Crosses
lineWinner [O, O, O] = Just Naughts
lineWinner _         = Nothing
