{-# LANGUAGE OverloadedStrings #-}

module SimulationSpec
    where

import Test.Hspec
import Situation hiding (change, new, view)
import Simulation
import Data.Aeson

spec :: SpecWith ()
spec = do
    let tof = "ToF"
        simWith name = pure new >>= add name
    describe "A simulation" $ do
        it "is initially empty" $ do
            let v = pure new
                  >>= view tof
            v `shouldBe` Left "no situation exists with name:ToF"

        it "can get the view of a situation once created" $ do
            let v = simWith tof >>= view tof
            v `shouldBe` Right (Halted, 15.0, 100)

        it "cannot add a situation with an already existing name" $ do
            let v = simWith tof >>= add tof >>= view tof
            v `shouldBe` Left "a situation already exists with name:ToF"

        it "can start a situation once created" $ do
            let v = simWith tof >>= apply start tof >>= view tof
            v `shouldBe` Right (Started, 15.0, 100)

        it "can change a situation once created" $ do
            let v = simWith tof >>= apply start tof >>= change 50 tof >>= view tof
            v `shouldBe` Right (Started, 15.0, 50)

        it "cannot change a situation that is not started" $ do
            let v = simWith tof >>= change 50 tof >>= view tof
            v `shouldBe` Left "situation for ToF is not started"

        it "can halt a situation once created" $ do
            let v = simWith tof >>= apply start tof >>= apply halt tof >>= view tof
            v `shouldBe` Right (Halted, 15.0, 100)

        it "can reset a situation once created" $ do
            let v = simWith tof >>= apply start tof >>= apply reset tof >>= view tof
            v `shouldBe` Right (Halted, 15.0, 100)

        it "can evolve a situation once created" $ do
            let v = simWith tof >>= apply start tof >>= apply evolve tof >>= view tof
            v `shouldBe` Right (Started, 14.0, 100)

        it "can start all situations" $ do
            let sim = simWith tof >>= add "Ben" >>= applyAll start
            (sim >>= view tof) `shouldBe` Right (Started, 15.0, 100)
            (sim >>= view "Ben") `shouldBe` Right (Started, 15.0, 100)

        it "can halt all situations" $ do
            let sim = simWith tof >>= add "Ben"
                     >>= applyAll start >>= applyAll halt
            (sim >>= view tof) `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= view "Ben") `shouldBe` Right (Halted, 15.0, 100)

        it "can evolve all situations that are started" $ do
            let sim = simWith tof >>= add "Ben" >>=  add "Gus"
                    >>= applyAll start >>= apply halt "Gus" >>= applyAll evolve
            (sim >>= view tof) `shouldBe` Right (Started, 14.0, 100)
            (sim >>= view "Ben") `shouldBe` Right (Started, 14.0, 100)
            (sim >>= view "Gus") `shouldBe` Right (Halted, 15.0, 100)

        it "can be encoded as JSON" $ do
            let sim = simWith tof
            encode sim `shouldBe`
                "{\"Right\":{\"situations\":{\"ToF\":{\"rooms\":[{\"temperatures\":[15,15,15,15,15],\"cursorPosition\":100}],\"state\":\"Halted\"}}}}"

