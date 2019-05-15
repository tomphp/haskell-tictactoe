{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module TicTacToe.Game
  ( Error
  , Result(..)
  , UI(..)
  , State
  , main
  , newState
  ) where

import Control.Monad.State  (MonadState, get, modify)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

-- State

data State = State { theBoard :: Board
                   , thePlayer :: Player
                   }

newState :: State
newState = State { theBoard = Board.new, thePlayer = Crosses }

player :: MonadState State m => m Player
player = thePlayer <$> get

board :: MonadState State m => m Board
board = theBoard <$> get

updatePlayer :: MonadState State m => (Player -> Player) -> m ()
updatePlayer = modify . updatePlayer'

updateBoard  :: MonadState State m => (Board -> Board) -> m ()
updateBoard = modify . updateBoard'

updatePlayer' :: (Player -> Player) -> State -> State
updatePlayer' update s@State{thePlayer=p} = s { thePlayer = update p }

updateBoard' :: (Board -> Board) -> State -> State
updateBoard' update s@State{theBoard=b} = s { theBoard = update b }

-- Error

data Error = BoardError Board.Error deriving Show

-- Main

main :: (MonadError Error m, MonadState State m, UI m, Monad m) => m ()
main = do
  whileM_ isInPlay playTurn
  gameOverScreen 
  where isInPlay = (== InPlay) <$> result

playTurn :: (MonadError Error m, MonadState State m, UI m) => m ()
playTurn = do
  turnScreen
  p <- getPositionInput
  setCell p `catchError` badPositionHandler
  switchPlayer

  where badPositionHandler (BoardError e) = do displayMessage $ tshow e
                                               displayMessage "Try again"
                                               playTurn

turnScreen :: (MonadState State m, UI m) => m ()
turnScreen = do
  b <- board
  displayBoard b
  p <- player
  displayMessage $ ""+|tshow p|+", "+|"choose cell:"

gameOverScreen :: (MonadState State m, UI m) => m ()
gameOverScreen = do
  b <- board
  displayBoard b
  r <- result
  displayMessage $ "Game over: "+|tshow r|+""

result :: MonadState State m => m Result
result = resultFromBoard <$> board

switchPlayer :: MonadState State m => m ()
switchPlayer = updatePlayer Player.switch

setCell :: (MonadError Error m, MonadState State m) => Int -> m ()
setCell position = do
  p <- player
  b <- board

  case Board.setCell (cell p) position b of
    Right b' -> updateBoard $ const b'
    Left e   -> throwError $ BoardError e

  where cell Naughts = Naught
        cell Crosses = Cross

-- UI

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board -> m ()
  getPositionInput :: m Int

-- Result

data Result = InPlay | Draw | Winner Player deriving (Eq)

instance Show Result where
  show InPlay     = "TerminalGame is in play"
  show Draw       = "Draw"
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
