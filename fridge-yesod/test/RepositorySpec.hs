{-# LANGUAGE OverloadedStrings #-}

module RepositorySpec
    where

import Test.Hspec
import Repository
import Simulation
import RoomView

spec :: SpecWith ()
spec = do
    describe "A repository" $ do
        it "is empty when created" $ do
            let r = newRepository
            findSimulation "ToF" r `shouldBe` Nothing

        it "can find a simulation after adding a name" $ do
            let r = add "ToF" newRepository
            findSimulation "ToF" r `shouldBe` Just (newSimulation "ToF")

        it "can evolve all its simulation" $ do
            let r = Repository.evolve (add "Gus" (add "ToF" newRepository))
            Simulation.roomView <$> (findSimulation "ToF" r) `shouldBe` Just (RoomView {temperature = 14.0, position = 100})
            Simulation.roomView <$> (findSimulation "Gus" r) `shouldBe` Just (RoomView {temperature = 14.0, position = 100})

