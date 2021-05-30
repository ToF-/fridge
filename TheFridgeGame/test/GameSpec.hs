module GameSpec where

import Test.Hspec
import Simulation
import Room
import Data.Function
import Game

spec = describe "game" $ do
    it "should initially contain no simulations" $ do
        let g = newGame
        simulationFor A newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: A"
        simulationFor C newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: C"

    it "should contain a simulation for a player after adding this player" $ do
        let g = addPlayer A newGame
        fmap (state . currentRoom) (simulationFor A g) `shouldBe` Right (15.0, 100)
        fmap (state . currentRoom) (simulationFor C g) `shouldBe` Left "NO EXISTING SIMULATION FOR: C"

    it "should give state for a simulation for a given player" $ do
        let g = addPlayer A newGame
        stateForPlayer A g `shouldBe` Right (15.0, 100)
        stateForPlayer C g `shouldBe` Left "NO EXISTING SIMULATION FOR: C"

    it "should give a message for a player in case something went wrong" $ do
        let g = newGame 
              & addPlayer A 
              & addPlayer B
              & startForPlayer A 
              & setPositionForPlayer 42 A 
              & setPositionForPlayer 50 B
        messageForPlayer A g `shouldBe` Nothing
        messageForPlayer B g `shouldBe` Just "SIMULATION NOT RUNNING"
        messageForPlayer E g `shouldBe` Just "NO EXISTING SIMULATION FOR: E"


    it "should allow for changing position for a given player" $ do
        let g = newGame & addPlayer A 
                        & addPlayer B 
                        & addPlayer C
                        & startForPlayer A
                        & startForPlayer B
                        & setPositionForPlayer 42 A
                        & setPositionForPlayer 58 C
        stateForPlayer A g `shouldBe` Right (15.0, 42)
        stateForPlayer B g `shouldBe` Right (15.0, 100)
        messageForPlayer C g `shouldBe` Just "SIMULATION NOT RUNNING"
        stateForPlayer D g `shouldBe` Left "NO EXISTING SIMULATION FOR: D"

    it "should allow for starting a simulation for a given player" $ do
        let g = addPlayer A newGame
            g'= startForPlayer A g
        fmap status (simulationFor A g') `shouldBe` Right Running

    it "should allow for stopping a simulation for a given player" $ do
        let g = addPlayer A newGame
            g'= stopForPlayer A (startForPlayer A g)
        fmap status (simulationFor A g') `shouldBe` Right Idle

    it "should update all simulations that are currently running" $ do
        let g = newGame & addPlayer A 
                        & addPlayer B 
                        & addPlayer C 
                        & startForPlayer A
                        & startForPlayer C
                        & updateRunningSimulations 
        stateForPlayer A g `shouldBe` Right (14.0, 100)
        stateForPlayer C g `shouldBe` Right (14.0, 100)
        stateForPlayer B g `shouldBe` Right (15.0, 100)

    it "should start all players" $ do
        let g = newGame & addPlayer A 
                        & addPlayer B 
                        & addPlayer C 
                        & startAll
        fmap status (simulationFor A g) `shouldBe` Right Running
        fmap status (simulationFor C g) `shouldBe` Right Running
        fmap status (simulationFor B g) `shouldBe` Right Running

    it "should stop all players" $ do
        let g = newGame & addPlayer A 
                        & addPlayer B 
                        & addPlayer C 
                        & startAll
                        & stopAll
        fmap status (simulationFor A g) `shouldBe` Right Idle
        fmap status (simulationFor C g) `shouldBe` Right Idle
        fmap status (simulationFor B g) `shouldBe` Right Idle
    
    it "should show all the simulations" $ do
        let g = newGame & addPlayer A 
                        & addPlayer B 
                        & startAll
                        & stopForPlayer B
        showAll g `shouldBe` ["A: Running Temp=15.0 Pos=100" ,"B: Idle Temp=15.0 Pos=100"]
