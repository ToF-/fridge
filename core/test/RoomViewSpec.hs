{-# LANGUAGE OverloadedStrings #-}

module RoomViewSpec
    where

import Test.Hspec
import Room (newRoom, evolve, change, open)
import RoomView
import Data.Aeson

spec :: SpecWith ()
spec = do
    describe "A room view" $ do
        it "can display the values of a room as viewed through user interface" $ do
            let room = evolve (change 50 (open newRoom))
                view = roomView room
            temperature view `shouldBe` 12.3
            position     view  `shouldBe` 50

        it "rounds the temperature value to one position" $ do
            let room = evolve (evolve (change 200 (open newRoom)))
                view = roomView room
            temperature view `shouldBe` 19.7


        it "can be encoded into json" $ do
            let room = evolve (change 50 (open newRoom))
                view = roomView room
            encode view `shouldBe`
                "{\"temperature\":12.3,\"position\":50}"

