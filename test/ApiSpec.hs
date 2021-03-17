{-# LANGUAGE OverloadedStrings #-}

module ApiSpec
    where

import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "API" $ do
        it "contains only a dummy test" $ do
            not False `shouldBe` True

