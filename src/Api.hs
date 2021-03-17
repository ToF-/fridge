{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Situation
import           Simulation
import           Web.Spock
import           Data.IORef

import           Data.Aeson       hiding (json)
import           GHC.Generics

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


app :: Api
app = do
    get "situations" $ do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation

    get ("situations" <//> var) $ \name -> do
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
            Right _ -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (simulation', simulation')
                json $ result >>= Simulation.getState name

    post "situations" $ do
        name <- jsonBody' :: ApiAction Name
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        let simulation' = simulation >>= addSituation name
        case simulation' of
            Left _ -> json $ simulation'
            Right _ -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (simulation', simulation')
                json $ result >>= Simulation.getState name

