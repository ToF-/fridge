module RoomSpec where

import Test.Hspec
import Room (position, room, state, temperature)

spec = describe "room" $ do
    it "should have a temperature and a position" $ do
        let r = room 15.0 100
        temperature r  `shouldBe` 15.0
        position r  `shouldBe` 100
    it "should have a state of the temperature (rounded) and position" $ do
        let r = room 1.33333333 42
        state r `shouldBe` (1.3, 42)

