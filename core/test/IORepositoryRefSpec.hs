module IORepositoryRefSpec
    where

import Test.Hspec
import IORepositoryRef
import Repository
import Simulation
import Control.Monad.IO.Class
import Data.IORef

spec :: SpecWith ()
spec = do
    describe "A IORepositoryRef" $ do
        it "is empty when created" $ do
            ref <- newIORepositoryRef
            rep <- readIORepositoryRef ref
            rep `shouldBe` newRepository

        it "can accept a new simulation for a name and retrieve it" $ do
            ref <- newIORepositoryRef
            create "ToF" ref
            sim <- retrieve "ToF" ref
            sim `shouldBe` Just (newSimulation "ToF")
            foo <- retrieve "Gus" ref
            foo `shouldBe` Nothing

        it "can update a simulation in the repository" $ do
            ref <- newIORepositoryRef
            create "ToF" ref
            update Repository.evolve ref
            update (Repository.change "ToF" 50) ref
            sim <- retrieve "ToF" ref
            sim `shouldBe` Just (Simulation.change 50 (Simulation.evolve (newSimulation "ToF")))

        it "can be saved as a JSON object" $ do
            ref <- newIORepositoryRef
            create "ToF" ref
            update Repository.evolve ref
            update (Repository.change "ToF" 50) ref
            save "myrepo.json" ref
            content <- readFile "myrepo.json"
            content `shouldBe`
                "[{\"room\":{\"temperatures\":[14,15,15,15,15],\"state\":\"Open\",\"position\":50},\"history\":[[1,14,100]],\"name\":\"ToF\"}]"





