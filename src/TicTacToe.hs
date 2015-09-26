import Data.List
import TicTacToe.Board
import TicTacToe.Player
import TicTacToe.GameLogic

play :: Move -> Board
play (board, player, position) = case player of
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
gameLoop board player = do
    drawBoard board
    putStr $ case player of
        Naughts -> "Naughts, "
        Crosses -> "Crosses, "
    putStr "choose cell: "
    position <- getLine
    evaluateState (play (board, player, pred $ read position))  player 

main :: IO ()
main = do
    putStrLn "Tic Tack Toe"
    gameLoop newBoard Crosses
