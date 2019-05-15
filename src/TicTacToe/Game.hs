{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}

module TicTacToe.Game
  ( Error
  , Result(..)
  , UI(..)
  , State
  , game
  , newState
  ) where

import Control.Lens         ((.=), (%=), makeLenses, use)
import Control.Monad.State  (MonadState)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

-- State

data State = State { _theBoard :: Board
                   , _thePlayer :: Player
                   }

makeLenses ''State

newState :: State
newState = State { _theBoard = Board.new, _thePlayer = Crosses }

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
  b <- use theBoard
  displayBoard b
  p <- use thePlayer
  displayMessage $ ""+|tshow p|+", "+|"choose cell:"

gameOverScreen :: (MonadState State m, UI m) => m ()
gameOverScreen = do
  b <- use theBoard
  displayBoard b
  r <- result
  displayMessage $ "Game over: "+|tshow r|+""

result :: MonadState State m => m Result
result = resultFromBoard <$> use theBoard

switchPlayer :: MonadState State m => m ()
switchPlayer = thePlayer %= Player.switch

setCell :: (MonadError Error m, MonadState State m) => Int -> m ()
setCell position = do
  p <- use thePlayer
  b <- use theBoard

  case Board.setCell (cell p) position b of
    Right b' -> theBoard .= b'
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
