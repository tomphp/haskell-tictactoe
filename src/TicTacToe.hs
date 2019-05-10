module TicTacToe (run) where 

import qualified TicTacToe.Game         as Game
import qualified TicTacToe.TerminalGame as TerminalGame

run :: IO ()
run = do
    putStrLn "Tic Tac Toe"
    TerminalGame.run Game.main



