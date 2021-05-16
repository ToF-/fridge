
{-# LANGUAGE OverloadedStrings #-}

module SimulationSpec
    where

import Test.Hspec
import Simulation
import RoomView (temperature, position)
import History

spec :: SpecWith ()
spec = do
    describe "A simulation" $ do
        it "is created with a name, has a new room, and a new history" $ do
            let sim = newSimulation "ToF"
            name sim `shouldBe` "ToF"
            temperature (roomView sim) `shouldBe` 15.0
            position (roomView sim) `shouldBe` 100
            history sim `shouldBe` newHistory


