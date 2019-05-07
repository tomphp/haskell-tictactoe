{-# LANGUAGE FlexibleContexts #-}
module TicTacToe (run) where 

import Control.Error.Safe (headZ, readZ)

import           TicTacToe.Board (Board, Cell(..))
import qualified TicTacToe.Board as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player
import           TicTacToe.GameLogic (GameState(..))
import qualified TicTacToe.GameLogic as GameLogic

play :: Board -> Player -> Int -> Board
play board Naughts position = Board.setCell board Naught position
play board Crosses position = Board.setCell board Cross position

drawBoard = putStrLn . tshow

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
    _              -> gameLoop board $ Player.switch player
  where state = GameLogic.getGameState board

gameLoop :: Board -> Player -> IO ()
gameLoop state player = do
    drawBoard state
    putStr (tshow player)
    putStr ", "
    putStrLn "choose cell: "
    (Just position) <- readZ . unpack <$> getLine
    evaluateState (play state player $ position - 1)  player 

run :: IO ()
run = do
    putStrLn "Tic Tack Toe"
    gameLoop Board.new Crosses
