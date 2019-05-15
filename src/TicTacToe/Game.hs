{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module TicTacToe.Game
  ( Error
  , Result(..)
  , UI(..)
  , State
  , game
  ) where

import Control.Lens         ((.=), (%=), use)
import Control.Monad.State  (MonadState)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player
import           TicTacToe.State  (State)
import qualified TicTacToe.State  as State

-- Error

data Error = BoardError Board.Error deriving Show

-- Main

game :: (MonadError Error m, MonadState State m, UI m, Monad m) => m ()
game = do
  whileM_ isInPlay playTurn
  gameOverScreen 
  where isInPlay = (== InPlay) <$> result

playTurn :: (MonadError Error m, MonadState State m, UI m) => m ()
playTurn = do
  turnScreen
  p <- getPositionInput
  setCell p `catchError` badPositionHandler
  switchPlayer

  where badPositionHandler (BoardError e) =
          do displayMessage $ tshow e
             displayMessage "Try again"
             playTurn

turnScreen :: (MonadState State m, UI m) => m ()
turnScreen = do
  b <- use State.board
  displayBoard b
  p <- use State.player
  displayMessage $ ""+|tshow p|+", "+|"choose cell:"

gameOverScreen :: (MonadState State m, UI m) => m ()
gameOverScreen = do
  b <- use State.board
  displayBoard b
  r <- result
  displayMessage $ "Game over: "+|tshow r|+""

result :: MonadState State m => m Result
result = resultFromBoard <$> use State.board

switchPlayer :: MonadState State m => m ()
switchPlayer = State.player %= Player.switch

setCell :: (MonadError Error m, MonadState State m) => Int -> m ()
setCell position = do
  p <- use State.player
  b <- use State.board

  case Board.setCell (cell p) position b of
    Right b' -> State.board .= b'
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
