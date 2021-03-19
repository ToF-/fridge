{-# LANGUAGE OverloadedStrings #-}

module SimulationSpec
    where

import Test.Hspec
import Situation
import Simulation
import Data.Aeson

spec :: SpecWith ()
spec = do
    describe "A simulation" $ do
        it "is initially empty" $ do
            let sim = newSimulation
            (stateForName "ToF" sim)
                `shouldBe` Left "no situation exists with name:ToF"

        it "can get the state of a situation once created" $ do
            let sim = addSituationForName "ToF" newSimulation
            (sim >>= (stateForName "ToF"))
                `shouldBe` Right (Halted, 15.0, 100)

        it "cannot add a situation with an already existing name" $ do
            let sim = addSituationForName "ToF" newSimulation
            (sim >>= addSituationForName "ToF")
                `shouldBe` Left "a situation already exists with name:ToF"

        it "can start a situation once created" $ do
            let sim = addSituationForName "ToF" newSimulation
            (sim >>= apply start "ToF" >>= stateForName "ToF")
                `shouldBe` Right (Started, 15.0, 100)
            (sim >>= apply start "Ben")
                `shouldBe` Left "no situation exists with name:Ben"

        it "can change a situation once created" $ do
            let sim = pure newSimulation
                      >>= addSituationForName "ToF"
                      >>= apply start "ToF"
                      >>= changeSituation 50 "ToF"
            (sim >>= stateForName "ToF")
                `shouldBe` Right (Started, 15.0, 50)
            (sim >>= changeSituation 50 "Ben")
                `shouldBe` Left "no situation exists with name:Ben"

        it "cannot change a situation that is not started" $ do
            let sim = addSituationForName "ToF" newSimulation
            (sim >>= changeSituation 50 "ToF" >>= stateForName "ToF")
                `shouldBe` Left "situation for ToF is not started"

        it "can halt a situation once created" $ do
            let sim = (addSituationForName "ToF" newSimulation) >>= apply start "ToF"
            (sim >>= apply halt "ToF" >>= stateForName "ToF") `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= apply halt "Ben") `shouldBe` Left "no situation exists with name:Ben"

        it "can reset a situation once created" $ do
            let sim = (addSituationForName "ToF" newSimulation) >>= apply start "ToF" >>= changeSituation 50 "ToF"
            (sim >>= apply reset "ToF" >>= stateForName "ToF") `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= apply reset "Ben") `shouldBe` Left "no situation exists with name:Ben"

        it "can tick a situation once created" $ do
            let sim = (addSituationForName "ToF" newSimulation) >>= apply start "ToF"
            (sim >>= apply tick "ToF" >>= stateForName "ToF") `shouldBe` Right (Started, 14.0, 100)
            (sim >>= apply tick "Ben") `shouldBe` Left "no situation exists with name:Ben"

        it "can start all situations" $ do
            let sim = return newSimulation >>= addSituationForName "ToF" >>= addSituationForName "Ben" >>= applyAll start
            (sim >>= stateForName "ToF") `shouldBe` Right (Started, 15.0, 100)
            (sim >>= stateForName "Ben") `shouldBe` Right (Started, 15.0, 100)

        it "can halt all situations" $ do
            let sim = return newSimulation >>= addSituationForName "ToF" >>= addSituationForName "Ben" >>= applyAll start >>= applyAll halt
            (sim >>= stateForName "ToF") `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= stateForName "Ben") `shouldBe` Right (Halted, 15.0, 100)

        it "can tick all situations that are started" $ do
            let sim = return newSimulation >>= addSituationForName "ToF" >>= addSituationForName "Ben" >>=  addSituationForName "Gus" >>= applyAll start >>= apply halt "Gus" >>= applyAll tick
            (sim >>= stateForName "ToF") `shouldBe` Right (Started, 14.0, 100)
            (sim >>= stateForName "Ben") `shouldBe` Right (Started, 14.0, 100)
            (sim >>= stateForName "Gus") `shouldBe` Right (Halted, 15.0, 100)

        it "can be encoded as JSON" $ do
            let sim = return newSimulation >>= addSituationForName "ToF"
            encode sim `shouldBe` "{\"Right\":{\"ToF\":{\"rooms\":[{\"temperatures\":[15,15,15,15,15],\"cursorPosition\":100}],\"state\":\"Halted\"}}}"
