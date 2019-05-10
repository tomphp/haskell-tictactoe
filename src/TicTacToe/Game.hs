module TicTacToe.Game
  ( State(..)
  , Result(..)
  , UI(..)
  , main
  , result
  ) where

import Control.Monad.Loops (whileM_)

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

main :: (State m, UI m, Monad m) => m ()
main = do
  whileM_ isInPlay playTurn
  gameOverScreen =<< result
  where isInPlay = (== InPlay) <$> result

playTurn :: (State m, UI m) => m ()
playTurn = do
  turnScreen
  position <- getPositionInput
  setCell position
  switchPlayer

class Monad m => State m where
  player       :: m Player
  board        :: m Board
  updatePlayer :: (Player -> Player) -> m ()
  updateBoard  :: (Board -> Board) -> m ()

class Monad m => UI m where
  turnScreen :: m ()
  gameOverScreen :: Result -> m ()
  getPositionInput :: m Int

result :: State m => m Result
result = resultFromBoard <$> board

switchPlayer :: State m => m ()
switchPlayer = updatePlayer Player.switch

setCell :: State m => Int -> m ()
setCell position = do
  p <- player
  updateBoard $ Board.setCell (cell p) position
  where cell Naughts = Naught
        cell Crosses = Cross

data Result = InPlay | Draw | Winner Player deriving (Eq)

instance Show Result where
  show InPlay          = "TerminalGame is in play"
  show Draw            = "Draw"
  show (Winner p) = show p <> " win"

resultFromBoard :: Board -> Result
resultFromBoard b = case winnerFromBoard b of
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
