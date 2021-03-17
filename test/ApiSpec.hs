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



