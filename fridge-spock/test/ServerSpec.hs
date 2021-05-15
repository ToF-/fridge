{-# LANGUAGE OverloadedStrings #-}

module ServerSpec
    where

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai.Test
import Control.Concurrent
import Web.Spock (spockAsApp)
import Data.String (fromString)
import Data.ByteString.Lazy.Char8 (unpack)

import Server

-- run a standard contain expectation in a WAI test session
shouldInclude :: SResponse -> String -> WaiSession () ()
shouldInclude response pattern = 
    liftIO $ (unpack (simpleBody response)) `shouldContain` pattern

spec :: Spec
spec = do
    with (spockAsApp (app 60)) $ do
        describe "GET /" $ do
            it "serves the main page" $ do
                response <- get "/"
                response `shouldInclude` "Welcome to the fridge game!"
                response `shouldInclude` "<input value=\"Start\" name=\"start\" type=\"submit\">"
                response `shouldInclude` "<input name=\"name\" id=\"room_name\" type=\"text\">"
                get "/" `shouldRespondWith` 200

        describe "POST /" $ do
            it "creates a new room" $ do
                response <- post "/" "name=Christophe&start=Start"
                liftIO $ print response
