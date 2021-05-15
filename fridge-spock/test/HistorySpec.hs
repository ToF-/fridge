{-# LANGUAGE OverloadedStrings #-}

module HistorySpec
    where

import Test.Hspec
import History
import Room
import Data.Aeson

spec :: SpecWith ()
spec = do
    describe "A history" $ do
        let r = open newRoom
            r' = evolve (change r 50)
            r''= evolve (change r' 75)
            h = add r'' $ add r' $ add r newHistory

        it "can store the successive room views accross time" $ do
            h `shouldBe` History [(1, 15.0, 100)
                                 ,(2, 12.3, 50)
                                 ,(3, 10.5, 75)]

        it "can be encoded into json" $ do
            encode h `shouldBe`
                "[[1,15,100],[2,12.3,50],[3,10.5,75]]"

        it "can tell its length" $ do
            lastMinute newHistory `shouldBe` 0
            lastMinute h `shouldBe` 3
