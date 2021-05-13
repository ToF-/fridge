{-# LANGUAGE OverloadedStrings #-}

module ServerSpec
    where

import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

import Server
spec :: Spec
spec = do
    with (spockAsApp (app 60)) $ do
        describe "GET /" $ do
            it "serves the initial page" $ do
                get "/" `shouldRespondWith`
                    "<head><link href=\"/css/main.css\" type=\"text/css\" rel=\"stylesheet\"><meta content=\"60\" http-equiv=\"Refresh\"></head><body><div class=\"center-div\"><h1>Welcome to the fridge game!</h1><input name=\"name\" type=\"text\"><input value=\"Start\" name=\"start\" type=\"submit\"></div></body>"
                        { matchStatus = 200 }
