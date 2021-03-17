{-# LANGUAGE OverloadedStrings #-}

module ApiSpec
    where

import Test.Hspec
import Test.Hspec.Wai
import           Control.Concurrent.Suspend
import Web.Spock (spockAsApp)
import Api

spec :: Spec
spec = do
    with (spockAsApp (app (sDelay 60))) $ do
        describe "GET /situations" $ do
            it "serves the situations, initially empty" $ do
                get "/situations" `shouldRespondWith` 
                    "{\"Right\":{}}" { matchStatus = 200 }

            it "serves a Left value when queried for non existing situation" $ do
                get "/situations/Ben" `shouldRespondWith` 
                    "{\"Left\":\"no situation exists with name:Ben\"}"
                        { matchStatus = 204 }

            it "serves a Right value when queried for an existing situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                get "/situations/Gus" `shouldRespondWith` 
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 200 }

        describe "POST /situations" $ do
            it "serves a Right value when creating a new situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
            it "serves a Left value when creating an already  existing situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Left\":\"a situation already exists with name:Gus\"}"
                        { matchStatus = 204 }

            it "serves a Right value when starting a situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                post "/situations/Gus" "{\"tag\":\"Start\"}" `shouldRespondWith`
                    "{\"Right\":[\"Started\",15.0,100]}"
                        { matchStatus = 202 }
                    
            it "serves a Right value when halting a situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                post "/situations/Gus" "{\"tag\":\"Start\"}" `shouldRespondWith`
                    "{\"Right\":[\"Started\",15.0,100]}"
                        { matchStatus = 202 }
                post "/situations/Gus" "{\"tag\":\"Halt\"}" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 202 }

            it "serves a Right value when changing a cursor position in a started situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                post "/situations/Gus" "{\"tag\":\"Start\"}" `shouldRespondWith`
                    "{\"Right\":[\"Started\",15.0,100]}"
                        { matchStatus = 202 }
                post "/situations/Gus" "{\"tag\":\"Change\", \"contents\":42 }" `shouldRespondWith`
                    "{\"Right\":[\"Started\",15.0,42]}"
                        { matchStatus = 202 }

            it "serves a Left value when changing a cursor position in a halted situation" $ do
                post "/situations" "\"Gus\"" `shouldRespondWith`
                    "{\"Right\":[\"Halted\",15.0,100]}"
                        { matchStatus = 201 }
                post "/situations/Gus" "{\"tag\":\"Change\", \"contents\":42 }" `shouldRespondWith`
                    "{\"Left\":\"situation for Gus is not started\"}"
                        { matchStatus = 400 }
