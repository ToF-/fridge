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

            it "serves a Left value when querying for non existant situation" $ do
                get "/situations/Ben" `shouldRespondWith` 
                    "{\"Left\":\"no situation exists with name:Ben\"}"
                        { matchStatus = 204 }

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
