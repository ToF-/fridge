{-# LANGUAGE OverloadedStrings #-}

module RoomSpec
    where

import Test.Hspec
import Room
import Data.Aeson

evolutions :: Int -> Room -> Room
evolutions 0 room = room
evolutions n room = evolutions (pred n) (evolve room)

openRoom = open newRoom

spec :: SpecWith ()
spec = do
    describe "A room" $ do
        it "can be created with initial values" $ do
            let room = openRoom
            temperature room `shouldBe` 15.0
            position room `shouldBe` 100

        it "can have its cursor position changed" $ do
            let room = change openRoom 20
            position room `shouldBe` 20

        it "cannot have its cursor position changed lower than zero" $ do
            let room = change openRoom (-1)
            position room `shouldBe` 0

        it "cannot have its cursor position changed higher than 200" $ do
            let room = change openRoom 201
            position room `shouldBe` 200

        it "has its temperature evolving at each evolution" $ do
            temperature openRoom `shouldBe` 15.0
            temperature (evolutions 1 openRoom) `shouldBe` 14.0
            temperature (evolutions 2 openRoom) `shouldBe` 13.0
            temperature (evolutions 3 openRoom) `shouldBe` 12.0
            temperature (evolutions 4 openRoom) `shouldBe` 11.0
            temperature (evolutions 5 openRoom) `shouldBe` 10.0
            temperature (evolutions 6 openRoom) `shouldBe` 9.333333333333334

        it "has its temperature evolving depending on cursor position" $ do
            temperature (evolve (change openRoom 50))
                `shouldBe` 12.333333333333334
            temperature (evolve (evolve (change openRoom 200)))
                `shouldBe` 19.666666666666664

        it "is initially in closed state" $ do
            state newRoom `shouldBe` Closed

        it "can be opened" $ do
            state (open newRoom) `shouldBe` Open

        it "can be closed" $ do 
            state (close (open newRoom)) `shouldBe` Closed

        it "cannot evolve or change in closed state" $ do
            temperature (evolve newRoom) `shouldBe` 15.0
            position (change newRoom 17) `shouldBe` 100

        it "cannot evolve or be changed in final state" $ do
            let room = close openRoom
            temperature (evolve newRoom) `shouldBe` 15.0
            position (change newRoom 17) `shouldBe` 100

