{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module TicTacToe.Game
  ( Error
  , Result(..)
  , State
  , game
  ) where

import Control.Lens         ((.=), (%=), use)
import Control.Monad.State  (MonadState)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player
import           TicTacToe.Result (Result(InPlay))
import qualified TicTacToe.Result as Result
import           TicTacToe.State  (State)
import qualified TicTacToe.State  as State
import           TicTacToe.UI     (UI)
import qualified TicTacToe.UI     as UI

data Error = BoardError Board.Error deriving Show

game :: (MonadError Error m, MonadState State m, UI m, Monad m) => m ()
game = do
  whileM_ isInPlay playTurn
  gameOverScreen 
  where isInPlay = (== InPlay) <$> result

playTurn :: (MonadError Error m, MonadState State m, UI m) => m ()
playTurn = do
  turnScreen
  p <- UI.getPositionInput
  setCell p `catchError` badPositionHandler
  switchPlayer

  where badPositionHandler (BoardError e) =
          do UI.displayMessage $ tshow e
             UI.displayMessage "Try again"
             playTurn

turnScreen :: (MonadState State m, UI m) => m ()
turnScreen = do
  b <- use State.board
  UI.displayBoard b
  p <- use State.player
  UI.displayMessage $ ""+|tshow p|+", "+|"choose cell:"

gameOverScreen :: (MonadState State m, UI m) => m ()
gameOverScreen = do
  b <- use State.board
  UI.displayBoard b
  r <- result
  UI.displayMessage $ "Game over: "+|tshow r|+""

result :: MonadState State m => m Result
result = Result.fromBoard <$> use State.board

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
