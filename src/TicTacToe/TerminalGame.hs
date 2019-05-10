{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.TerminalGame (run) where

import Control.Error.Safe (readZ)
import Control.Monad.Loops (untilJust)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, modify)

import           TicTacToe.Game      (Game(..), UI(..))
import qualified TicTacToe.Game      as Game
import           TicTacToe.Board     (Board, Cell(..))
import qualified TicTacToe.Board     as Board
import           TicTacToe.Player    (Player(..))
import qualified TicTacToe.Player    as Player

newtype TerminalGame m a = TerminalGame { runTerminalGame :: StateT TheState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TheState)

data TheState = TheState { theBoard :: Board
                         , thePlayer :: Player
                         }

run :: Monad m => TerminalGame m a -> m a
run game = evalStateT (runTerminalGame game) newState

newState :: TheState
newState = TheState { theBoard = Board.new, thePlayer = Crosses }

instance Monad m => Game (TerminalGame m) where
  player = thePlayer <$> get

  board = theBoard <$> get

  switchPlayer = modify $ updatePlayer Player.switch

  setCell position = do
    p <- player
    modify $ updateBoard $ Board.setCell (cell p) position

updatePlayer :: (Player -> Player) -> TheState -> TheState
updatePlayer update s@TheState{thePlayer=p} = s { thePlayer = update p }

updateBoard :: (Board -> Board) -> TheState -> TheState
updateBoard update s@TheState{theBoard=b} = s { theBoard = update b }

cell :: Player -> Cell
cell Naughts = Naught
cell Crosses = Cross

instance MonadIO m => UI (TerminalGame m) where
  gameOverScreen state = do
    b <- Game.board
    drawBoard b
    putStr "TerminalGame over: "
    putStrLn (tshow state)

  turnScreen = do
    b <- Game.board
    p <- player
    drawBoard b
    putStr (tshow p)
    putStr ", "
    putStrLn "choose cell: "

  getPositionInput =
    untilJust $ do
      pos <- readZ . unpack <$> getLine
      when (isNothing pos) $ putStrLn "Try again..."
      return $ pos

drawBoard :: MonadIO m => Board -> m ()
drawBoard = putStrLn . tshow
