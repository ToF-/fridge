{-# LANGUAGE OverloadedStrings #-}
module GraphDataSpec
    where

import Test.Hspec
import GraphData
import Simulation
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS

spec :: SpecWith ()
spec = do
    describe "Graph Data" $ do
        it "is generated from a Simulation" $ do
            let sim = evolve $ change 50 $ evolve $ newSimulation "ToF"
            let gd = graphData sim
            encode gd `shouldBe` 
                "{\"data\":{\"values\":[{\"temperature\":14,\"minute\":1,\"position\":10},{\"temperature\":11.3,\"minute\":2,\"position\":5}]},\"layer\":[{\"mark\":{\"point\":true,\"type\":\"line\"},\"encoding\":{\"color\":{\"value\":\"#dc3a17\"},\"x\":{\"field\":\"minute\",\"type\":\"quantitative\"},\"y\":{\"field\":\"temperature\",\"type\":\"quantitative\"}}},{\"mark\":{\"point\":true,\"type\":\"line\"},\"encoding\":{\"color\":{\"value\":\"#1792dc\"},\"x\":{\"field\":\"minute\",\"type\":\"quantitative\"},\"y\":{\"field\":\"position\",\"type\":\"quantitative\"}}}]}"

