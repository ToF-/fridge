module RoomSpec
    where

import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Dummy" $ do
        it "has a spec" $ do
            (2+2) `shouldBe` (4 :: Int)
