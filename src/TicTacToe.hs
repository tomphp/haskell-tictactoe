module TicTacToe (run) where 

import Data.List
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
cellToChar :: (Cell, Int) -> Char
cellToChar cell = case cell of
    (Naught, _)     -> 'O'
    (Cross, _)      -> 'X'
    (Empty, number) -> head $ show number

-- Side effecting
drawBoard :: Board -> IO ()
drawBoard board = do
    putStrLn $ formatRow $ take 3 parsedBoard
    putStrLn "---------"
    putStrLn $ formatRow $ take 3 $ drop 3 parsedBoard
    putStrLn "---------"
    putStrLn $ formatRow $ drop 6 parsedBoard
    putStrLn ""
    where parsedBoard = map cellToChar $ zip board [1..9]
          formatRow row = intersperse ' ' $ intersperse '|' row

gameOver :: Board -> String -> IO ()
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
    position <- getLine
    evaluateState (play state player $ (read position) - 1)  player 

run :: IO ()
run = do
    putStrLn "Tic Tack Toe"
    gameLoop newBoard Crosses
