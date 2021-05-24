module Main where

import Server
import GHC.Int                             (Int64)
import Control.Concurrent.Suspend          (sDelay)
import Control.Concurrent.Timer            (repeatedTimer)
import System.Environment                  (getArgs)
import System.Exit                         (exitFailure)
import IORepositoryRef                     (IORepositoryRef
                                           , newIORepositoryRef
                                           , update
                                           , save)
import Repository                          (evolve)

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

repeatedAction :: IORepositoryRef -> IO ()
repeatedAction ref = do
    putStrLn "updating all rooms"
    update evolve ref
    putStrLn "saving simulations"
    save "simulations.json" ref

main :: IO ()
main = do
    found <- getDelay
    case found of
      Nothing -> exitFailure
      Just delay -> do
        ref <- newIORepositoryRef
        _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
        putStrLn "launching fridge-server"
        serve ref
