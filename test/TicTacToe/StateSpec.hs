module TicTacToe.StateSpec where

import Test.Hspec

import Control.Lens ((^.))

import qualified TicTacToe.Board  as Board
import           TicTacToe.Player (Player(..))
import           TicTacToe.State  (board, player)
import qualified TicTacToe.State  as State

spec :: Spec
spec = describe "TicTacToe.State" $
  describe "new" $ do
    let state = State.new

    it "should initialise the player to Crosses" $
      (state^.player) `shouldBe` X
  
    it "should initialise the board to an empty board" $
      (state^.board) `shouldBe` Board.empty
