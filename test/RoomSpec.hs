module RoomSpec
    where

import Test.Hspec
import Room

evolutions :: Int -> Room -> Room
evolutions 0 room = room
evolutions n room = evolutions (pred n) (evolve room)

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

        it "has its temperature evolving at each evolution" $ do
            temperature newRoom `shouldBe` 15.0
            temperature (evolutions 1 newRoom) `shouldBe` 14.0
            temperature (evolutions 2 newRoom) `shouldBe` 13.0
            temperature (evolutions 3 newRoom) `shouldBe` 12.0
            temperature (evolutions 4 newRoom) `shouldBe` 11.0
            temperature (evolutions 5 newRoom) `shouldBe` 10.0
            temperature (evolutions 6 newRoom) `shouldBe` 9.333333333333334

        it "has its temperature evolving depending on cursor position" $ do
            temperature (evolve (setCursorPosition newRoom 50)) 
                `shouldBe` 12.333333333333334
            temperature (evolve (evolve (setCursorPosition newRoom 200))) 
                `shouldBe` 19.666666666666664
