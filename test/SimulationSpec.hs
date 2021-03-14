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




