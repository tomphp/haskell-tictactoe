{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module TicTacToe.Game
  ( Error
  , State(..)
  , Result(..)
  , UI(..)
  , main
  ) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

data Error = BoardError Board.Error deriving Show

main :: (MonadError Error m, State m, UI m, Monad m) => m ()
main = do
  whileM_ isInPlay playTurn
  gameOverScreen 
  where isInPlay = (== InPlay) <$> result

playTurn :: (MonadError Error m, State m, UI m) => m ()
playTurn = do
  turnScreen
  p <- getPositionInput
  setCell p `catchError` badPositionHandler
  switchPlayer

  where badPositionHandler (BoardError e) = do displayMessage $ tshow e
                                               displayMessage "Try again"
                                               playTurn


turnScreen :: (State m, UI m) => m ()
turnScreen = do
  b <- board
  displayBoard b
  p <- player
  displayMessage $ ""+|tshow p|+", "+|"choose cell:"

gameOverScreen :: (State m, UI m) => m ()
gameOverScreen = do
  b <- board
  displayBoard b
  r <- result
  displayMessage $ "Game over: "+|tshow r|+""

class Monad m => State m where
  player       :: m Player
  board        :: m Board
  updatePlayer :: (Player -> Player) -> m ()
  updateBoard  :: (Board -> Board) -> m ()

class Monad m => UI m where
  displayMessage :: Text -> m ()
  displayBoard :: Board -> m ()
  getPositionInput :: m Int

result :: State m => m Result
result = resultFromBoard <$> board

switchPlayer :: State m => m ()
switchPlayer = updatePlayer Player.switch

setCell :: (MonadError Error m, State m) => Int -> m ()
setCell position = do
  p <- player
  b <- board

  case Board.setCell (cell p) position b of
    Right b' -> updateBoard $ const b'
    Left e   -> throwError $ BoardError e

  where cell Naughts = Naught
        cell Crosses = Cross

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
