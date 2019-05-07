module TicTacToe.PlayerSpec where

import Test.Hspec

import           TicTacToe.Player (Player(..))
import qualified TicTacToe.Player as Player

spec :: Spec
spec = describe "TicTacToe.Player" $ do
  describe "show" $
    it "shows the player" $ do
      show Naughts `shouldBe` "Naughts"
      show Crosses `shouldBe` "Crosses"

  describe "switch" $
    it "switch players" $ do
      Player.switch Naughts `shouldBe` Crosses
      Player.switch Crosses `shouldBe` Naughts
