module TicTacToe.GameSpec where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)

import Test.Hspec

import           TicTacToe.State (State)
import qualified TicTacToe.Game  as Game

newtype TestGame m a = TestGame { runTestGame :: StateT State (ExceptT Game.Error m) a }

spec :: Spec
spec = do
  describe "TicTacToe.Game" $ do
    describe "game" $ do
      it "something" $ do
        pending

    describe "playTurn" $ do
      it "something" $ do
        pending

    describe "gameOverScreen" $ do
      it "something" $ do
        pending

    -- context "getGameState" $ do
    --   it "is in play if there are empty cells and no winning lines" $ do
    --     let board = Board [ Naught, Cross,  Empty
    --                       , Naught, Cross,  Cross
    --                       , Cross,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` InPlay

    --   it "is a draw if all cells are taken and there are no winning lines" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Naught, Cross,  Cross
    --                       , Cross,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Draw

    --   it "has been won be crosses if the there is a winning line of crosses" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Cross,  Cross,  Cross
    --                       , Empty,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Winner Crosses
         

    --   it "has been won be naughts if the there is a winning line of naughts" $ do
    --     let board = Board [ Naught, Cross,  Naught
    --                       , Cross,  Naught, Cross
    --                       , Empty,  Naught, Naught
    --                       ]
    --     GameLogic.getGameState board `shouldBe` Winner Naughts
