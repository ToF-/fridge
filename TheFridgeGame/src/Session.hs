module Session where
import Data.Char
import Room
import Game
import Simulation
import Control.Concurrent

data Command = Quit
             | List
             | Help
             | Add PlayerId
             | Display PlayerId
             | Pos PlayerId Position 
             | Run
             | End
             | Start PlayerId
             | Break PlayerId
    deriving (Eq, Show, Read)

type GameResult = (Game, [String])

command :: String -> Maybe Command
command s = interpret (unwords (expandFirst (words s))) 

interpret :: String -> Maybe Command
interpret s = case reads (normalize s)  of
                [(cmd,_)] -> Just cmd
                [] -> Nothing

expandFirst :: [String] -> [String]
expandFirst [] = []
expandFirst (w:ws) = expand w : ws

expand :: String -> String
expand s = case [k | 
                 k <- keywords, 
                 (normalize s) == take (length s) k] of
    [k] -> k
    [] -> s
keywords = words "Quit List Help Add Display Pos Run End Start Break"

normalize :: String -> String
normalize = unwords . capitalizeFirst . words
capitalizeFirst :: [String] -> [String]
capitalizeFirst [] = []
capitalizeFirst (w:ws) = capitalize w : ws

capitalize :: String -> String
capitalize [] = []
capitalize (c:s) = toUpper c : map toLower s

prompt :: Monad m => (String -> m ()) -> m () 
prompt out = out "Quit | List | Run | End | Add \"id\" | Start \"id\" | Break \"id\" | Display \"id\" | Pos \"id\" n\n"

entry :: Monad m => (m String) -> m (Maybe Command)
entry imp = fmap command imp 

doCommand :: Game -> (Maybe Command) -> GameResult
doCommand g cmd = case cmd of
                  Just Help -> (g, help)
                  Just Quit -> (g, ["Bye"])
                  Just List -> (g, showAll g)
                  Just (Add playerId) -> addNewPlayer playerId g
                  Just (Start playerId) -> startPlayer playerId g
                  Just (Break playerId) -> stopPlayer playerId g
                  Just Run -> go g
                  Just End -> halt g
                  Just (Display playerId) -> playerDisplay playerId g
                  Just (Pos playerId n) -> playerSetPosition playerId n g
                  Nothing -> (g,["???"])


addNewPlayer :: PlayerId -> Game -> GameResult
addNewPlayer playerId g = case stateForPlayer playerId g of
                         Left _ -> (addPlayer playerId g, ["Player " ++ show playerId ++ " added to the game."])
                         Right _ -> (g, ["Player " ++ show playerId ++ " is already in the game."])


startPlayer :: PlayerId -> Game -> GameResult
startPlayer playerId g = let g' = startForPlayer playerId g in
                             case stateForPlayer playerId g of
                               Right _ -> (g',["Starting simulation for "++ show playerId])
                               Left _ -> (g, ["Player " ++ show playerId ++ " is not in the game."])

stopPlayer :: PlayerId -> Game -> GameResult
stopPlayer playerId g = let g' = stopForPlayer playerId g in
                             case stateForPlayer playerId g of
                               Right _ -> (g',["Pausing simulation for "++show playerId])
                               Left _ -> (g, ["Player " ++ show playerId ++ " is not in the game."])

playerDisplay :: PlayerId -> Game -> GameResult
playerDisplay playerId g = case stateForPlayer playerId g of
                           Right (t,p) -> (g, ["Display for " ++ show playerId ++ ": " ++ (show t) ++ " " ++ (show p)])
                           Left _ -> (g, ["Player " ++ show playerId ++ " is not in the game."])

playerSetPosition :: PlayerId -> Position -> Game -> (Game,[String])
playerSetPosition playerId p g = let g' = setPositionForPlayer p playerId g
    in case stateForPlayer playerId g' of
         Right _ -> (g',["Player "++show playerId++" set position to "++(show p)])
         Left msg -> (g, [msg])

go :: Game -> GameResult 
go g = (startAll g, [ "Starting all simulations" ])

halt :: Game -> GameResult
halt g = (stopAll g, ["Pausing all simulations"])

pureGameLoop :: Monad m => Game -> (m String) -> (String -> m ()) -> m ()
pureGameLoop g inp out = do
    prompt out
    x <- entry inp    
    let (g',msg) = doCommand g x
    out (unlines msg)
    if x == Just Quit then return () else pureGameLoop g' inp out

concurrentGameLoop :: MVar Game -> IO ()
concurrentGameLoop mvar = do
    g <- takeMVar mvar
    prompt putStrLn
    x <- entry getLine
    let (g', msg) = doCommand g x
    putStrLn (unlines msg)
    putMVar mvar g'
    if x == Just Quit then return () else concurrentGameLoop mvar 

updateConcurrentGame :: MVar Game -> IO ()
updateConcurrentGame mvar = do
    g <- takeMVar mvar
    let g' = updateRunningSimulations g
    putMVar mvar g'

help :: [String]
help = ["Help : display this message"
       ,"Quit : quit the game"
       ,"Add     [ABCDEF] : add a player in the game"
       ,"Start   [ABCDEF] : start/resume simulation for a player"
       ,"Pos     [ABCDEF] <0--100> : set position for a player"
       ,"Display [ABCDEF] : display the state of a player"
       ,"Break   [ABCDEF] : pause simulation for a player"
       ,"Run : start/resume simulation for all players"
       ,"End : pause simulation for all players"
       ,"List : display the state of all players"]
