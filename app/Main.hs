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
type ApiAction a = SpockAction () () AppState a

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
    get ("situation" <//> var) $ \name -> do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation >>= Simulation.getState name

    post "situation" $ do
        name <- jsonBody' :: ApiAction Name
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        let simulation' = simulation >>= addSituation name
        case simulation' of
            Left _ -> json $ simulation'
            Right sim -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (simulation', simulation')
                json $ result >>= Simulation.getState name

