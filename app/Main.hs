{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Simulation
import           Web.Spock
import           Web.Spock.Config
import           Data.IORef

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import           Control.Monad.Trans (liftIO)

data AppState = AppState (IORef (Either String Simulation))

type Api = SpockM () () AppState ()

main :: IO ()
main = do
    ref <- newIORef (return newSimulation >>= addSituation "ToF")
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    runSpock 8080 (spock spockCfg app)

app :: Api
app = do
    get "simulation" $ do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation
    get ("state" <//> var) $ \name -> do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation >>= Simulation.getState name
