module TicTacToe.Game
  ( Game(..)
  , GameState(..)
  , UI(..)
  , main
  , state
  ) where

import Control.Monad.Loops (whileM_)

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))


main :: (Game m, UI m, Monad m) => m ()
main = do
  whileM_ isInPlay playTurn
  gameOverScreen =<< state
  where isInPlay = (== InPlay) <$> state

playTurn :: (Game m, UI m) => m ()
playTurn = do
  turnScreen
  position <- getPositionInput
  setCell position
  switchPlayer

class Monad m => Game m where
  player       :: m Player
  board        :: m Board
  switchPlayer :: m ()
  setCell      :: Int -> m ()

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: GameState -> m ()
  getPositionInput :: m Int

state :: Game m => m GameState
state = stateFromBoard <$> board

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
lineWinner [Cross, Cross, Cross]    = Just Crosses
lineWinner [Naught, Naught, Naught] = Just Naughts
lineWinner _                        = Nothing
