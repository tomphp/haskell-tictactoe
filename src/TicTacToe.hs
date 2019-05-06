{-# LANGUAGE FlexibleContexts #-}
module TicTacToe (run) where 

import Control.Error.Safe (headZ, readZ)
import Data.List.Split (chunksOf)
import TicTacToe.Board
import TicTacToe.Player
import TicTacToe.GameLogic

switchPlayer :: Player -> Player
switchPlayer player = case player of
    Naughts -> Crosses
    Crosses -> Naughts

play :: Board -> Player -> Int -> Board
play board player position = case player of
    Naughts -> setCell board Naught position
    Crosses -> setCell board Cross position

-- Rendering
cellToChar :: (Cell, Int) -> Text
cellToChar cell = case cell of
    (Naught, _)     -> "O"
    (Cross, _)      -> "X"
    (Empty, number) -> tshow number

-- Side effecting
drawBoard :: Board -> IO ()
drawBoard board =
  putStrLn rendered
    where
      rendered    = intercalate "\n----------\n" formatted
      formatted   = map formatRow rows
      rows        = chunksOf 3 parsedBoard
      parsedBoard = zipWith (curry cellToChar) board [1..9]
      formatRow   = unwords . intersperse "|"

gameOver :: Board -> Text -> IO ()
gameOver board message = do
  drawBoard board
  putStr "Game over: "
  putStrLn message

evaluateState :: Board -> Player -> IO ()
evaluateState board player = case state of
  Draw           -> gameOver board "Draw"
  Winner Crosses -> gameOver board "Crosses win"
  Winner Naughts -> gameOver board "Naughts win"
  _              -> gameLoop board $ switchPlayer player
  where state = getGameState board

gameLoop :: Board -> Player -> IO ()
gameLoop state player = do
    drawBoard state
    putStr $ case player of
        Naughts -> "Naughts, "
        Crosses -> "Crosses, "
    putStrLn "choose cell: "
    (Just position) <- readZ . unpack <$> getLine
    evaluateState (play state player $ position - 1)  player 

run :: IO ()
run = do
    putStrLn "Tic Tack Toe"
    gameLoop newBoard Crosses
