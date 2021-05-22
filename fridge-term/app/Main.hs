module Main where

import Control.Monad
import Control.Concurrent.Suspend    (sDelay)
import Control.Concurrent.Timer      (repeatedTimer)
import Command
import IORepositoryRef
import Repository
import Terminal

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

main :: IO ()
main = do
    putStrLn "welcome to fridge-term"
    ref <- newIORepositoryRef
    _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
    forever (process ref)
