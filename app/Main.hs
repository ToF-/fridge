{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Situation
import           Simulation
import           Api
import           Web.Spock
import           Web.Spock.Config
import           Data.IORef

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Control.Concurrent.Suspend
import           Control.Concurrent.Timer

import           Control.Monad.Trans (liftIO)


main :: IO ()
main = do
    ref <- newIORef (return newSimulation >>= addSituation "ToF")
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    repeatedTimer (repeatedAction ref) (sDelay 10)
    runSpock 8080 (spock spockCfg app)

repeatedAction :: IORef (Either String Simulation) -> IO ()
repeatedAction ref = do
    simulation <- readIORef ref
    let simulation' = simulation >>= applyAll tick
    atomicModifyIORef' ref $ const (simulation',simulation')
    return ()

