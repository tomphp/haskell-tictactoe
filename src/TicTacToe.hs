
import TicTacToe.Board
import TicTacToe.Player
import TicTacToe.GameLogic
import TicTacToe.Rendering

-- TODO - introduce new states
generateDna :: Board -> Player -> GameState
generateDna board player = (InPlay, board, player)

-- TODO - refactor in TicTacToe.GameLogic.play
makePlay :: Board -> Player -> CellIndex -> GameState
makePlay board player cell= (getPlayState newBoard, newBoard, switchPlayer player)
  where newBoard = play (board, player, cell) 

handleInput :: String -> Board -> Player -> GameState
handleInput input board player = case input of
  "g" -> generateDna board player
  _   -> makePlay board player (pred $ read input)

-- Side effecting
drawBoard :: Board -> IO ()
drawBoard board = putStrLn $ boardToString board

gameOver :: String -> IO ()
gameOver message = do
  putStr "Game over: "
  putStrLn message

displayState :: GameState -> IO ()
displayState (state, board, player) = case state of
  Draw           -> gameOver "Draw"
  Winner Crosses -> gameOver "Crosses win"
  Winner Naughts -> gameOver "Naughts win"
  _              -> gameLoop board player

gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
  putStr $ case player of
      Naughts -> "Naughts, "
      Crosses -> "Crosses, "
  putStr "choose cell: "
  input <- getLine
  let state = handleInput input board player
  let (_, newBoard, _) = state
  drawBoard newBoard
  displayState state

main :: IO ()
main = do
  putStrLn "Tic Tack Toe"
  let board = newBoard
  drawBoard board
  gameLoop board Crosses
  putStrLn "Tic Tack Toe"
