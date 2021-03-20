module ServerSpec
    where

import Test.Hspec
import Network.HTTP.Types.Status
import Server
import Simulation

spec :: SpecWith ()
spec = do
    describe "the server" $ do
        it "initially has status OK, empty message, and empty simulation" $ do
            let server = newServer
            status server `shouldBe` status200
            message server `shouldBe` ""
            simulation server `shouldBe` new

        it "returns a 204 when queried for an non existing name" $ do
            let server = getSituation "ToF" newServer
            status server  `shouldBe` status204
            message server `shouldBe` "no situation exists with name:ToF"
            simulation_state server `shouldBe` Nothing

