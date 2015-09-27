module TicTacToe.GameLogic where

import Data.List (nub)
import TicTacToe.Board
import TicTacToe.Player

data PlayState = InPlay | Draw | Winner Player deriving (Eq, Show)

type GameState = (PlayState, Board, Player)

type IndexLine = [CellIndex]
type CellLine  = [Cell]

type Move = (Board, Player, CellIndex)

gameLines :: [IndexLine]
gameLines = [ [0, 1, 2], [3, 4, 5], [6, 7, 8]
            , [0, 3, 6], [1, 4, 7], [2, 5, 8]
            , [0, 4, 8], [2, 4, 6]
            ]
           
cellLine :: Board -> IndexLine -> CellLine
cellLine board = map (board !!)

getWinner :: CellLine -> Maybe Player
getWinner line = case nub line of
  [Cross]  -> Just Crosses
  [Naught] -> Just Naughts
  _        -> Nothing

lineResults :: Board -> [Maybe Player]
lineResults board = map (getWinner . cellLine board) gameLines

getWinnerFromBoard :: Board -> Maybe Player
getWinnerFromBoard board = foldr combineWinner Nothing $ lineResults board
  where combineWinner carry line = case carry of Nothing -> line
                                                 _       -> carry

getPlayState :: Board -> PlayState
getPlayState board = case getWinnerFromBoard board of
  Just player -> Winner player
  _           -> if Empty `elem` board then InPlay else Draw

switchPlayer :: Player -> Player
switchPlayer player = case player of
    Naughts -> Crosses
    Crosses -> Naughts

play :: Move -> Board
play (board, player, position) = setCell board cell position
  where cell = case player of Naughts -> Naught 
                              Crosses -> Cross 
