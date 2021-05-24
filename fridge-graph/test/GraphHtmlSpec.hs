{-# LANGUAGE OverloadedStrings #-}
module GraphHtmlSpec
    where

import GraphHtml
import Test.Hspec
import Simulation
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

spec :: SpecWith ()
spec = do
    describe "Graph Html" $ do
        it "is generated from a Simulation" $ do
            let sim = evolve $ change 50 $ evolve $ newSimulation "ToF"
            let html = graphHtml sim
            putStrLn (renderHtml html)
            (renderHtml html) `shouldBe`
                "<!DOCTYPE HTML>\n<html><head><script src=\"https://cdn.jsdelivr.net/npm/vega@5.20.2\"></script><script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5.1.0\"></script><script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6.17.0\"></script></head><body><h1><span>Temperatures for ToF</span></h1><div id=\"vis\"><script>var vlSpec = {\"data\":{\"values\":[{\"temperature\":14,\"minute\":1,\"position\":10},{\"temperature\":11.3,\"minute\":2,\"position\":5}]},\"layer\":[{\"mark\":{\"point\":true,\"type\":\"line\"},\"encoding\":{\"color\":{\"value\":\"#dc3a17\"},\"x\":{\"field\":\"minute\",\"type\":\"quantitative\"},\"y\":{\"field\":\"temperature\",\"type\":\"quantitative\"}}},{\"mark\":{\"point\":true,\"type\":\"line\"},\"encoding\":{\"color\":{\"value\":\"#1792dc\"},\"x\":{\"field\":\"minute\",\"type\":\"quantitative\"},\"y\":{\"field\":\"position\",\"type\":\"quantitative\"}}}]}; vegaEmbed('#vis', vlSpec);</script></div></body></html>"
