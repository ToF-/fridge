module RoomSpec
    where

import Test.Hspec
import Room

spec :: SpecWith ()
spec = do
    describe "A room" $ do
        it "can be created with initial values" $ do
            let room = newRoom
            temperature room `shouldBe` 15.0
            cursorPosition room `shouldBe` 100

        it "can have its cursor position changed" $ do
            let room = setCursorPosition newRoom 20
            cursorPosition room `shouldBe` 20
                

