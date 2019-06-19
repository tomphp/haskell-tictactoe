module TicTacToe.TerminalGame (run) where

import Control.Error.Safe         (readZ)
import Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import Control.Monad.Loops        (untilJust)
import Control.Monad.Reader       (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (MonadState, get, put)
import Data.List.Split            (chunksOf)

import qualified TicTacToe.Board      as Board
import qualified TicTacToe.Coordinate as Coordinate
import qualified TicTacToe.Game       as Game
import           TicTacToe.Player     (Player(..))
import           TicTacToe.State      (State)
import qualified TicTacToe.State      as State
import           TicTacToe.UI         (UI(..))

type Env = IORef State

newtype TerminalGame m a =
  TerminalGame { runTerminalGame :: ReaderT Env (ExceptT Game.Error m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Env
           , MonadError Game.Error
           )

run :: MonadIO m => TerminalGame m a -> m a
run game = do
  ref <- newIORef State.new
  result <- runExceptT $ runReaderT (runTerminalGame game) ref
  either (error "Error happened") return result

instance MonadIO m => MonadState State (TerminalGame m) where
  get = ask >>= readIORef
  put s = ask >>= \ref -> writeIORef ref s

-- UI

instance MonadIO m => UI (TerminalGame m) where
  displayMessage = putStrLn

  displayBoard = putStrLn . Board.render boardRenderer

  getPositionInput =
    untilJust $ do
      idx <- getNumberInput
      let coordinate = Coordinate.fromIndex idx
      when (isNothing coordinate) $
        displayMessage "Attempting to set a cell which does not exist"
      return coordinate

getNumberInput :: (MonadIO m, UI m) => m Int
getNumberInput =
  untilJust $ do
    pos <- readZ . unpack <$> getLine
    when (isNothing pos) $ displayMessage "Try again..."
    return pos

boardRenderer :: [Maybe Player] -> Text
boardRenderer cs = rendered
  where
    rendered    = intercalate "\n---------\n" formatted
    formatted   = map formatRow rs
    rs          = chunksOf 3 parsedBoard
    parsedBoard = zipWith (curry cellToChar) cs [1..9]
    formatRow   = unwords . intersperse "|"

cellToChar :: (Maybe Player, Int) -> Text
cellToChar (Just O,  _     ) = "O"
cellToChar (Just X,  _     ) = "X"
cellToChar (Nothing, number) = tshow number
