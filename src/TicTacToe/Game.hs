module TicTacToe.Game
  ( Game(..)
  , GameState(..)
  , gameIsRunning
  , state
  ) where

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))

class Monad m => Game m where
  player       :: m Player
  board        :: m Board
  switchPlayer :: m ()
  setCell      :: Int -> m ()

state :: Game m => m GameState
state = stateFromBoard <$> board

gameIsRunning :: Game m => m Bool
gameIsRunning = (== InPlay) <$> state

data GameState = InPlay | Draw | Winner Player deriving (Eq)

instance Show GameState where
  show InPlay          = "TerminalGame is in play"
  show Draw            = "Draw"
  show (Winner p) = show p <> " win"

stateFromBoard :: Board -> GameState
stateFromBoard b = case winnerFromBoard b of
  Just p  -> Winner p
  Nothing -> if Empty `elem` Board.cells b then InPlay else Draw

winnerFromBoard :: Board -> Maybe Player
winnerFromBoard = foldr combineWinner Nothing . lineResults
  where combineWinner Nothing line = line
        combineWinner carry   _    = carry

lineResults :: Board -> [Maybe Player]
lineResults = map lineWinner . Board.lines

lineWinner :: [Cell] -> Maybe Player
lineWinner line = case line of
  [Cross, Cross, Cross]    -> Just Crosses
  [Naught, Naught, Naught] -> Just Naughts
  _                        -> Nothing
