{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Room
import Simulation
import History
import GraphHtml
import Data.Aeson
import Text.Blaze.Html.Renderer.String
import Data.ByteString.Lazy.Char8 as BS

instance FromJSON Room
instance FromJSON RoomState
instance FromJSON History
instance FromJSON Simulation

main :: IO ()
main = do
    content <- BS.readFile "simulation.json"
    let found = decode content
    case found of
      Nothing -> Prelude.putStrLn "uh oh"
      Just sim -> Prelude.putStrLn $ renderHtml $ graphHtml sim
