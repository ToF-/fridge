module SituationSpec
    where

import Test.Hspec
import Situation
import Room

spec :: SpecWith ()
spec = do
    describe "A situation" $ do
        it "has a room" $ do
            temperature (room (newSituation)) `shouldBe` 15.0
