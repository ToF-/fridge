module HistorySpec where
import Test.Hspec
import Room
import History
import Data.Function

spec = describe "history" $ do
    it "should record past states of a room" $ do
        let h = record (room 14.0 50)
                (record (room 15.0 100) 
                    emptyHistory)
            r = h `at` (-1) 
            r'= h `at` (-2)
        temperature r `shouldBe` 14.0
        temperature r' `shouldBe` 15.0
        position r  `shouldBe` 50
        position r'  `shouldBe` 100

    it "should give initial values as default" $ do
        let h = emptyHistory
        temperature (h `at` (-100))  `shouldBe` 15.0
        position (h `at` (-100))  `shouldBe` 100

    it "should report all room states" $ do
        let h = emptyHistory & record (room 15.0 100)
                             & record (room 14.0 40)
                             & record (room 23.0 42)
        report h `shouldBe` [(0, 15.0, 100)
                            ,(1, 14.0,  40)
                            ,(2, 23.0,  42)]

