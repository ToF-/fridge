{-# LANGUAGE OverloadedStrings #-}

module SituationSpec
    where

import Test.Hspec
import Situation
import Room
import Data.Aeson

spec :: SpecWith ()
spec = do
    describe "A situation" $ do
        it "has a room" $ do
            temperature (room (newSituation)) `shouldBe` 15.0

        it "is initially in halted state" $ do
            state newSituation `shouldBe` Halted

        it "can be started and evolve" $ do
            state (start newSituation) `shouldBe` Started
            temperature (room (tick (start newSituation))) `shouldBe` 14.0

        it "cannot evolve unless started" $ do
            temperature (room (tick newSituation)) `shouldBe`
                temperature (room newSituation)

        it "can be halted" $ do
            state (halt (start newSituation)) `shouldBe` Halted
            temperature (room (tick (halt (start newSituation)))) `shouldBe` 15.0

        it "has an history" $ do
            let s = start newSituation
                s' = tick s
                s''= tick s'
                h = history s''
            h!!0 `shouldBe` room s
            h!!1 `shouldBe` room s'
            h!!2 `shouldBe` room s''

        it "can be reset" $ do
            let s = reset (tick (tick (tick (start newSituation))))
            state s `shouldBe` Halted
            length (history s) `shouldBe` 1
            temperature (room s) `shouldBe` 15.0

        it "can be encoded into json" $ do
            encode newSituation `shouldBe`
                "{\"rooms\":[{\"temperatures\":[15,15,15,15,15],\"cursorPosition\":100}],\"state\":\"Halted\"}"

        it "can have its room cursor position changed" $ do
            let s = change 50 (start newSituation)
            encode s `shouldBe` 
                "{\"rooms\":[{\"temperatures\":[15,15,15,15,15],\"cursorPosition\":50}],\"state\":\"Started\"}"


        it "cannot have its room cursor position changed unless started" $ do
            let s = change 50 newSituation
            encode s `shouldBe` 
                "{\"rooms\":[{\"temperatures\":[15,15,15,15,15],\"cursorPosition\":100}],\"state\":\"Halted\"}"

