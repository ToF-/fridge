module SimulationSpec where

import Test.Hspec
import Room (state,temperature)
import History
import Simulation (currentRoom, newSimulation, setPosition, start, status, stop, Status (Idle,Running), update)

spec = describe "simulation" $ do
    it "should initially be idle, not running" $ do
        fmap status newSimulation `shouldBe` Right Idle

    it "should be running once started" $ do
        let simulation = newSimulation >>= start
        fmap status simulation `shouldBe` Right Running

    it "should initially have a room with a temperature of 15 and a position of 100" $ do
        fmap (state . currentRoom) newSimulation `shouldBe` Right (15.0, 100)

    it "should not evolve if it is not in a running state" $ do
        let simulation = newSimulation >>= update
        fmap (state . currentRoom) simulation  `shouldBe` Right (15.0, 100)

    it "should evolve in temperature after update" $ do
        let simulation = newSimulation >>= start >>= update
            simulation'= simulation >>= update
        fmap (temperature . currentRoom) simulation  `shouldBe` Right 14 
        fmap (temperature . currentRoom) simulation' `shouldBe` Right 13

    it "should have its temperature decreasing differently after 5 updates" $ do
        let simulation = newSimulation >>= start
            updates    = take 7 (iterate (>>= update) simulation)
            fromRight (Right x) = x
            temp       = fst . state . currentRoom . fromRight
        map temp updates `shouldBe` [15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.3]

    it "should not allow to set position if not running" $ do
        let simulation = newSimulation >>= setPosition 50
        simulation `shouldBe` Left "SIMULATION NOT RUNNING"

    it "should not allow a position not in the range 0..200" $ do
        let simulation = newSimulation >>= start >>= setPosition (-1)
        simulation `shouldBe` Left "INCORRECT POSITION"
        let simulation = newSimulation >>= start >>= setPosition 201
        simulation `shouldBe` Left "INCORRECT POSITION"
   
    it "should allow its position to be modified once running" $ do
        let simulation = newSimulation >>= start >>= setPosition 50
        fmap (state . currentRoom) simulation `shouldBe` Right (15.0, 50)

    it "should be stoppable" $ do
        let simulation = newSimulation >>= start >>= stop
        fmap status simulation `shouldBe` Right Idle

    it "should show its current info" $ do
        let simulation = newSimulation >>= start
        fmap show simulation `shouldBe` Right "Running Temp=15.0 Pos=100" 
