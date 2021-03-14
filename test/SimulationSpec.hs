{-# LANGUAGE OverloadedStrings #-}

module SimulationSpec
    where

import Test.Hspec
import Situation
import Simulation

spec :: SpecWith ()
spec = do
    describe "A simulation" $ do
        it "is initially empty" $ do
            let sim = newSimulation
            (getState "ToF" sim) `shouldBe` Left "no situation exists with name:ToF"

        it "can get the state of a situation once created" $ do
            let sim = addSituation "ToF" newSimulation
            (sim >>= (getState "ToF")) `shouldBe` Right (Halted, 15.0, 100)

        it "cannot add a situation with an already existing name" $ do
            let sim = addSituation "ToF" newSimulation 
            (sim >>= addSituation "ToF") `shouldBe` Left "a situation already exists with name:ToF" 

        it "can start a situation once created" $ do
            let sim = addSituation "ToF" newSimulation
            (sim >>= startSituation "ToF" >>= getState "ToF") `shouldBe` Right (Started, 15.0, 100)
            (sim >>= startSituation "Ben") `shouldBe` Left "no situation exists with name:Ben"

        it "can change a situation once created" $ do
            let sim = addSituation "ToF" newSimulation
            (sim >>= startSituation "ToF" >>= changeSituation "ToF" 50 >>= getState "ToF") 
                `shouldBe` Right (Started, 15.0, 50)
            (sim >>= changeSituation "Ben" 50) `shouldBe` Left "no situation exists with name:Ben"

        it "cannot change a situation that is not started" $ do
            let sim = addSituation "ToF" newSimulation
            (sim >>= changeSituation "ToF" 50 >>= getState "ToF") 
                `shouldBe` Left "situation for ToF is not started"

        it "can halt a situation once created" $ do
            let sim = (addSituation "ToF" newSimulation) >>= startSituation "ToF"
            (sim >>= haltSituation "ToF" >>= getState "ToF") `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= haltSituation "Ben") `shouldBe` Left "no situation exists with name:Ben"

        it "can reset a situation once created" $ do
            let sim = (addSituation "ToF" newSimulation) >>= startSituation "ToF" >>= changeSituation "ToF" 50
            (sim >>= resetSituation "ToF" >>= getState "ToF") `shouldBe` Right (Halted, 15.0, 100)
            (sim >>= resetSituation "Ben") `shouldBe` Left "no situation exists with name:Ben"

