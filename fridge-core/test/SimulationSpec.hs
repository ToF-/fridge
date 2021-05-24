{-# LANGUAGE OverloadedStrings #-}

module SimulationSpec
    where

import Test.Hspec
import Simulation
import RoomView (temperature, position)
import History
import Data.Aeson

spec :: SpecWith ()
spec = do
    describe "A simulation" $ do
        it "is created with a name, has a new room, and a new history" $ do
            let sim = newSimulation "ToF"
            name sim `shouldBe` "ToF"
            temperature (roomView sim) `shouldBe` 15.0
            position (roomView sim) `shouldBe` 100
            history sim `shouldBe` newHistory

        it "can evolve its immediately after creation" $ do
            let sim = evolve (newSimulation "ToF")
            temperature (roomView sim) `shouldBe` 14.0

        it "can change its position" $ do
            let sim = change 50 (newSimulation "ToF")
            position (roomView sim) `shouldBe` 50

        it "add to its history when evolving" $ do
            let sim = evolve (evolve (newSimulation "ToF"))
            let History ls = history sim
            length ls `shouldBe` 2

        it "close the room and stop evolving after 25 evolutions" $ do
            let sim = last (take 26 (iterate evolve (newSimulation "ToF")))
            let History ls = history sim
            length ls `shouldBe` 25
            let sim' = evolve sim
            let History ls'= history sim'
            ls'  `shouldBe` ls

        it "can be encoded in json" $ do
            let sim = evolve (change 150 (evolve (newSimulation "ToF")))
            encode sim `shouldBe`
                "{\"room\":{\"temperatures\":[14.666666666666666,14,15,15,15],\"state\":\"Open\",\"position\":150},\"history\":[[1,14,100],[2,14.7,150]],\"name\":\"ToF\"}"
