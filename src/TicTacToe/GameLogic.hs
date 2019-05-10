module TicTacToe.GameLogic
  ( GameState(..)
  , getGameState
  ) where

import TicTacToe.Board
import qualified TicTacToe.Board  as Board
import TicTacToe.Player

data GameState = InPlay | Draw | Winner Player deriving (Eq)

instance Show GameState where
  show InPlay          = "Game is in play"
  show Draw            = "Draw"
  show (Winner player) = show player <> " win"

getGameState :: Board -> GameState
getGameState board = case getWinnerFromBoard board of
  Just player -> Winner player
  _           -> if Empty `elem` cells board then InPlay else Draw

getWinnerFromBoard :: Board -> Maybe Player
getWinnerFromBoard board = foldr combineWinner Nothing $ lineResults board
  where combineWinner Nothing line = line
        combineWinner carry   _    = carry

lineResults :: Board -> [Maybe Player]
lineResults = map getWinner . Board.lines

getWinner :: [Cell] -> Maybe Player
getWinner line = case line of
  [Cross, Cross, Cross]    -> Just Crosses
  [Naught, Naught, Naught] -> Just Naughts
  _                        -> Nothing
