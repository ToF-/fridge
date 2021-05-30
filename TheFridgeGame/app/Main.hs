import System.Exit
import Session
import Game
import Control.Concurrent
import Control.Concurrent.Timer (newTimer, repeatedStart, stopTimer)
import Control.Concurrent.Suspend.Lifted (Delay, sDelay)
import System.Environment

validateArgs :: [String] -> IO ()
validateArgs ss 
   | length ss == 1 && isInteger (ss !! 0) = return ()
validateArgs _   = do putStrLn "USAGE: TerminalSession <delay in seconds>"
                      exitWith (ExitFailure 1)

isInteger :: String -> Bool
isInteger s = case reads s :: [(Int,String)] of
                [(n,_)] -> True
                [] -> False
                  
main :: IO ()
main = do 
    args <- getArgs
    validateArgs args 
    let d = sDelay (read (args !! 0))
    mvar <- newMVar newGame
    t <- newTimer
    repeatedStart t (updateConcurrentGame mvar) d
    concurrentGameLoop mvar 
    exitSuccess
