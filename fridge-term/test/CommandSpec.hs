module CommandSpec
    where

import Test.Hspec
import Command

spec :: SpecWith ()
spec = do
    describe "there are commands for" $ do
        it "adding a new room for a given name" $ do
            command "add ToF" `shouldBe`
                (Right $ Add "ToF")

        it "displaying a room" $ do
            command "display ToF" `shouldBe`
                (Right $ Display "ToF")

        it "changing the position in a room" $ do
            command "change ToF 50" `shouldBe`
                (Right $ Change "ToF" 50)

        it "displaying all rooms" $ do
            command "rooms" `shouldBe`
                (Right $ Rooms)

        it "displaying a simulation" $ do
            command "simulation ToF" `shouldBe`
                (Right $ Simulation "ToF")

        it "other than that a command is not recognized" $ do
            command "foo bar    qux" `shouldBe`
                (Left "unknown command: foo bar qux")

    describe "commands can be recognized" $ do
        it "when partially entered" $ do
            command "a ToF"  `shouldBe` (Right $ Add "ToF")
            command "sim Gus" `shouldBe` (Right $ Simulation "Gus")
            command "DisP ToF" `shouldBe` (Right $ Display "ToF")
            command "ch Gus 50" `shouldBe` (Right $ Change "Gus" 50)
            command "R" `shouldBe` (Right $ Rooms)

    describe "some commands require arguments:" $ do
        it "add, display, change and simulation require a name" $ do
            command "add" `shouldBe` Left "missing name"
            command "d" `shouldBe` Left "missing name"
            command "c" `shouldBe` Left "missing name"
            command "s" `shouldBe` Left "missing name"
        it "change requires also a number" $ do
            command "change ToF" `shouldBe` Left "missing number"
            command "change ToF foo" `shouldBe` Left "not a number: foo"
        it "change requires a number between 0 and 200" $ do
            command "change ToF 201" `shouldBe` Left "number too large"
            command "change ToF -1" `shouldBe` Left "number too small"



