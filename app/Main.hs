{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Situation
import           Simulation
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

data AppState = AppState (IORef (Either String Simulation))

data Command = Start | Halt | Reset | Change Int
    deriving (Eq, Show, Generic)

instance ToJSON Command
instance FromJSON Command

data Action = Action Command Name
    deriving (Eq, Show, Generic)

type Api = SpockM () () AppState ()
type ApiAction a = SpockAction () () AppState a

main :: IO ()
main = do
    ref <- newIORef (return newSimulation >>= addSituation "ToF")
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    repeatedTimer (repeatedAction ref) (sDelay 10)
    runSpock 8080 (spock spockCfg app)

repeatedAction :: IORef (Either String Simulation) -> IO ()
repeatedAction ref = do
    simulation <- readIORef ref
    case simulation of
        Left _ -> return ()
        Right sim -> do
            let simulation' = simulation >>= applyAll tick
            atomicModifyIORef' ref $ const (simulation',simulation')
            return ()

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

    post ("action" <//> var) $ \name -> do
        command <- jsonBody' :: ApiAction Command
        let f = case command of
                  Start -> apply start
                  Halt -> apply halt
                  Reset -> apply reset
                  (Change n) -> (changeSituation n)
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        let simulation' = simulation >>= f name
        case simulation' of
            Left _ -> json $ simulation'
            Right sim -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (simulation', simulation')
                json $ result >>= Simulation.getState name

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

