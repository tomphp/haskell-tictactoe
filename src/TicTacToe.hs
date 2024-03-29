module TicTacToe (run) where

import AppPrelude

import qualified TicTacToe.Game         as Game
import qualified TicTacToe.TerminalGame as TerminalGame

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    TerminalGame.run Game.game
