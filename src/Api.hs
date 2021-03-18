{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Situation
import           Simulation
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid
import           Data.IORef

import           Data.Aeson       hiding (json)
import           GHC.Generics

import           Control.Concurrent.Suspend
import           Control.Concurrent.Timer
import           Control.Monad.Trans (liftIO)
import           Network.Wai (Middleware)
import Network.HTTP.Types.Status

data AppState = AppState (IORef (Either String Simulation))

data Command = Start | Halt | Reset | Change Int
    deriving (Eq, Show, Generic)

instance ToJSON Command
instance FromJSON Command

data Action = Action Command Name
    deriving (Eq, Show, Generic)

type Api = SpockM () () AppState ()
type ApiAction a = SpockAction () () AppState a

repeatedAction :: IORef (Either String Simulation) -> IO ()
repeatedAction ref = do
    simulation <- readIORef ref
    let simulation' = simulation >>= applyAll tick
    _ <- atomicModifyIORef' ref $ const (simulation',simulation')
    return ()

app :: Delay -> IO Middleware
app delay = do
    ref <- newIORef (return newSimulation)
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    _ <- repeatedTimer (repeatedAction ref) delay
    spock spockCfg routes


routes :: Api
routes = do
    get "situations" $ do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation

    get ("situations" <//> var) $ \name -> do
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= getSimulationState name
        let st = case newSim of
                   Left _ -> status204
                   Right _ -> status200
        setStatus st
        json $ newSim

    post ("situations" <//> var) $ \name -> do
        command <- jsonBody' :: ApiAction Command
        let f = case command of
                  Start -> apply start
                  Halt -> apply halt
                  Reset -> apply reset
                  (Change n) -> (changeSituation n)
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= f name
        case newSim of
            Left _ -> do
                setStatus status400
                json $ newSim
            Right _ -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (newSim, newSim)
                setStatus status202
                json $ result >>= getSimulationState name

    post "situations" $ do
        name <- jsonBody' :: ApiAction Name
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= addSituation name
        case newSim of
            Left _ -> do
                setStatus status204
                json $ newSim
            Right _ -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (newSim, newSim)
                setStatus status201
                json $ result >>= getSimulationState name

