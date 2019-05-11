{-# LANGUAGE FlexibleContexts #-}

module TicTacToe.Game
  ( State(..)
  , Result(..)
  , UI(..)
  , main
  ) where

import Control.Monad.Except (catchError)
import Control.Monad.Loops  (whileM_)

import Fmt

import           TicTacToe.Board  (Board, Cell(..))
import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

main :: (State m, UI m, Monad m) => m ()
main = do
  whileM_ isInPlay playTurn
  gameOverScreen 
  where isInPlay = (== InPlay) <$> result

playTurn :: (State m, UI m) => m ()
playTurn = do
  turnScreen
  p <- getPositionInput
  setCell p
  switchPlayer

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

setCell :: State m => Int -> m ()
setCell position = do
  p <- player
  updateBoard $ updater p
  where cell Naughts = Naught
        cell Crosses = Cross

        updater p b = let (Right res) = Board.setCell (cell p) position b `catchError` (\_ -> error "ouch")
                      in res

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
