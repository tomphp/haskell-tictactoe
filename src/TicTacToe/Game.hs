module TicTacToe.Game
  ( Error
  , game
  , playTurn
  , gameOverScreen
  ) where

import AppPrelude

import Control.Lens         (use, (%=), (.=))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (iterateUntil)
import Control.Monad.State  (MonadState)

import Fmt

import qualified TicTacToe.Board       as Board
import           TicTacToe.Coordinate (Coordinate)
import           TicTacToe.Player     (Player(..))
import qualified TicTacToe.Player     as Player
import           TicTacToe.Result     (Result(..))
import qualified TicTacToe.Result     as Result
import           TicTacToe.State      (State)
import qualified TicTacToe.State      as State
import           TicTacToe.UI         (UI)
import qualified TicTacToe.UI         as UI

newtype Error = BoardError Board.Error deriving Show

game :: (MonadError Error m, MonadState State m, UI m) => m ()
game = iterateUntil Result.isGameOver playTurn >>= gameOverScreen

-- Play turn

playTurn :: (MonadError Error m, MonadState State m, UI m) => m (Result Player)
playTurn = do
  placeToken
  switchPlayer
  result

placeToken :: (MonadError Error m, MonadState State m, UI m) => m ()
placeToken = do
  turnScreen
  c <- UI.getPositionInput
  setCell c `catchError` badPositionHandler

result :: MonadState State m => m (Result Player)
result = Result.fromBoard <$> use State.board

badPositionHandler :: (MonadError Error m, MonadState State m, UI m) => Error -> m ()
badPositionHandler (BoardError e) = do
  UI.displayMessage $ errorMsg e
  UI.displayMessage "Try again"
  void placeToken

turnScreen :: (MonadState State m, UI m) => m ()
turnScreen = do
  b <- use State.board
  UI.displayBoard b
  p <- use State.player
  UI.displayMessage $ ""+|tshow p|+", "+|"choose cell:"

switchPlayer :: MonadState State m => m ()
switchPlayer = State.player %= Player.switch

setCell :: (MonadError Error m, MonadState State m) => Coordinate -> m ()
setCell coordinate = do
  p <- use State.player
  b <- use State.board

  case Board.setCell p coordinate b of
    Right b' -> State.board .= b'
    Left e   -> throwError $ BoardError e

errorMsg :: Board.Error -> Text
errorMsg Board.CellIsNotEmpty   =  "Attempting to set a cell which is not empty"

-- Game Over

gameOverScreen :: (UI m) => Result Player -> m ()
gameOverScreen r = do
  UI.displayBoard (Result.board r)
  UI.displayMessage $ "Game over: "+|resultMsg r|+""

resultMsg :: Result Player -> Text
resultMsg (InPlay _)   = "TerminalGame is in play"
resultMsg (Draw _)     = "Draw"
resultMsg (Winner _ p) = tshow p <> " win"
