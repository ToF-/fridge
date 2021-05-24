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

        it "can change a simulation for a name" $ do
            let r = add "ToF" newRepository
                r'= Repository.change "ToF" 50 r
            Simulation.roomView <$> (findSimulation "ToF" r) `shouldBe`Just (RoomView {temperature = 15.0, position = 100})

        it "can list all its simulations" $ do
            let r = add "Gus" (add "ToF" newRepository)
            simulations r `shouldBe` [newSimulation "Gus", newSimulation "ToF"]
