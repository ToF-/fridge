module SessionSpec where

import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import Session
import Game
import Data.Function

spec = describe "session" $ do
    describe "command" $ do
        it "should recognize a command" $ do
            command "Add A" `shouldBe` Just (Add A)
            command "Display A" `shouldBe` Just (Display A)
            command "foo" `shouldBe` Nothing
            command "Run"   `shouldBe` Just Run
            command "End" `shouldBe` Just End
            command "List" `shouldBe` Just List
            command "Pos A 42" `shouldBe` Just (Pos A 42) 
            command "Quit" `shouldBe` Just Quit
            command "Start A" `shouldBe` Just (Start A) 
            command "Break A" `shouldBe` Just (Break A) 

    describe "help" $ do
        it "should display help" $ do
            (doCommand newGame (Just Help)) `shouldBe` (newGame,
                ["Help : display this message"
                ,"Quit : quit the game"
                ,"Add     [ABCDEF] : add a player in the game"
                ,"Start   [ABCDEF] : start/resume simulation for a player"
                ,"Pos     [ABCDEF] <0--100> : set position for a player"
                ,"Display [ABCDEF] : display the state of a player"
                ,"Break   [ABCDEF] : pause simulation for a player"
                ,"Run : start/resume simulation for all players"
                ,"End : pause simulation for all players"
                ,"List : display the state of all players"])


    describe "prompt" $ do
        it "should display a prompt" $ do
            let out = \s -> writer ((), s)
                run = prompt out
            (snd (runWriter run)) `shouldBe` "Quit | List | Run | End | Add \"id\" | Start \"id\" | Break \"id\" | Display \"id\" | Pos \"id\" n\n"

    describe "entry" $ do
        it "should read an entry and recognize a command" $ do
            let imp = return "Pos A 42\n"
            cmd <- entry imp 
            cmd `shouldBe` Just (Pos A 42) 

        it "should recognize only a part of the keyword in a command" $ do
            let imp = return "p A 42\n"
            cmd <- entry imp 
            cmd `shouldBe` Just (Pos A 42) 

    describe "doCommand" $ do 
        it "should execute a command on a game and yield a new game and messages" $ do
            (doCommand newGame (Just Quit)) `shouldBe` (newGame, ["Bye"])


        it "should notify if the command is not correct" $ do
            doCommand newGame Nothing `shouldBe` (newGame, ["???"])

        it "should add a player if not already added" $ do
            let (g',msg) = doCommand newGame $ Just $ Add A
            msg  `shouldBe` ["Player A added to the game."]
            let (g'',msg) = doCommand g' $ Just $ Display A
            msg  `shouldBe` ["Display for A: 15.0 100"]
            let (g'',msg) = doCommand g' $ Just $ Add A
            msg  `shouldBe` ["Player A is already in the game."]
            g''  `shouldBe` g'

        it "should display an error if asked the state for a non player" $ do
            let (g',msg) = doCommand newGame $ Just $ Display A
            msg  `shouldBe` ["Player A is not in the game."]
            g' `shouldBe` newGame

        it "should list all the simulations in the game" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (_,msg)= doCommand g'' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"
                            ,"B: Idle Temp=15.0 Pos=100"]
                           
        it "should start the simulation for a player" $ do
            let (g,_) = doCommand newGame $ Just $ Add A 
            let (g',msg)= doCommand g $ Just $ Start A
            msg `shouldBe` ["Starting simulation for A"]
            let (_,msg)= doCommand g' $ Just List
            msg  `shouldBe` ["A: Running Temp=15.0 Pos=100"]

        it "should stop the simulation for a player" $ do
            let (g,_) = doCommand newGame $ Just $ Add A 
            let (g',msg)= doCommand g $ Just $ Start A
            let (g'',msg)= doCommand g' $ Just $ Break A
            msg `shouldBe` ["Pausing simulation for A"]
            let (_,msg)= doCommand g'' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"]

        it "should start the simulation for all players" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (g''',msg)=doCommand g'' $ Just Run
            msg `shouldBe` ["Starting all simulations"]
            
            let (_,msg)= doCommand g''' $ Just List
            msg  `shouldBe` ["A: Running Temp=15.0 Pos=100"
                            ,"B: Running Temp=15.0 Pos=100"]
            
            
        it "should stop the simulation for all players" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (g''',_)=doCommand g'' $ Just Run
            let (g'''',msg)=doCommand g''' $ Just End
            msg `shouldBe` ["Pausing all simulations"]
            
            let (_,msg)= doCommand g'''' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"
                            ,"B: Idle Temp=15.0 Pos=100"]

        it "should set a position for a player if that player has started" $ do
            let g = newGame & addPlayer A & startForPlayer A 
            let (g',msg) = doCommand g $ Just $ Pos A 42
            msg `shouldBe` ["Player A set position to 42"]


