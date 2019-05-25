module TicTacToe.PlayerSpec where

import Test.Hspec

import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

spec :: Spec
spec = describe "TicTacToe.Player" $ do
  describe "show" $
    it "shows the player" $ do
      show O `shouldBe` "Naughts"
      show X `shouldBe` "Crosses"

  describe "switch" $
    it "switch players" $ do
      Player.switch O `shouldBe` X
      Player.switch X `shouldBe` O
