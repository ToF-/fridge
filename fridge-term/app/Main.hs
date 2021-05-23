module Main where

import Control.Monad
import Control.Concurrent.Suspend    (sDelay)
import Control.Concurrent.Timer      (repeatedTimer)
import GHC.Int
import System.Environment
import Command
import IORepositoryRef
import Repository
import Terminal
import System.Exit

process :: IORepositoryRef -> IO ()
process ref = do
    s <- getLine
    let found = command s
    case found of
      Left msg -> putStrLn msg
      Right cmd -> execute cmd ref

delay = 10

repeatedAction :: IORepositoryRef -> IO ()
repeatedAction ref = do
    putStrLn "updating all rooms"
    update evolve ref
    putStrLn "saving simulations"
    save "simulations.json" ref

getDelay :: IO (Maybe GHC.Int.Int64)
getDelay = do
    args <- getArgs
    case length args < 1 of
      True -> do
          putStrLn "setting evolution delay to 60 seconds"
          return (Just 60)
      False -> do
          case reads (args !! 0) of
            [] -> do
                putStrLn "argument must be a number between 10 and 60"
                return Nothing
            ((n,_):_) -> if (n >= 10) && (n <= 60)
                            then do
                                putStrLn ("setting evolution delay to " <> (show n) <>" seconds")
                                return (Just n)
                            else do
                                putStrLn "argument must be a number between 10 and 60"
                                return Nothing

main :: IO ()
main = do
    found <- getDelay
    case found of
      Nothing -> exitFailure
      Just delay -> do
        putStrLn "welcome to fridge-term"
        ref <- newIORepositoryRef
        _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
        forever (process ref)
