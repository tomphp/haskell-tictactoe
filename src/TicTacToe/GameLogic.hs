module TicTacToe.GameLogic where

import TicTacToe.Board
import TicTacToe.Player

data GameState = InPlay | Draw | Winner Player deriving (Eq, Show)

type IndexLine = [Int]
type CellLine  = [Cell]

gameLines :: [IndexLine]
gameLines = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
             [0, 3, 6], [1, 4, 7], [2, 5, 8],
             [0, 4, 8], [2, 4, 6] ]
           
cellLine :: Board -> IndexLine -> CellLine
cellLine board indexes = map (board!!) indexes

getWinner :: CellLine -> Maybe Player
getWinner line = case line of
  [Cross, Cross, Cross]    -> Just Crosses
  [Naught, Naught, Naught] -> Just Naughts
  _                        -> Nothing

lineResults board = map (getWinner . (cellLine board)) gameLines

getWinnerFromBoard :: Board -> Maybe Player
getWinnerFromBoard board = foldr combineWinner Nothing $ lineResults board
  where combineWinner carry line = case carry of Nothing -> line
                                                 _       -> carry

getGameState :: Board -> GameState
getGameState board = case getWinnerFromBoard board of
  Just player -> Winner player
  _           -> if elem Empty board then InPlay else Draw
