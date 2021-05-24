{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module GraphHtml
    where

import Simulation
import GraphData
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Aeson
import Data.ByteString.Lazy.Char8


graphHtml :: Simulation -> Html
graphHtml sim = docTypeHtml $ do
    H.head $ do
        H.script ! A.src "https://cdn.jsdelivr.net/npm/vega@5.20.2" $ toHtml (""::String)
        H.script ! A.src "https://cdn.jsdelivr.net/npm/vega-lite@5.1.0" $ toHtml (""::String)
        H.script ! A.src "https://cdn.jsdelivr.net/npm/vega-embed@6.17.0" $ toHtml (""::String)
    H.body $ do
        H.h1 $ H.span $ toHtml ("Temperatures for " <> Simulation.name sim)
        H.div ! A.id "vis" $ do
            H.script $ do
                toHtml ( "var vlSpec = " <> (unpack (encode (graphData sim))) <> "; vegaEmbed('#vis', vlSpec);")


