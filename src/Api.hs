{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Concurrent.Suspend    (sDelay)
import Control.Concurrent.Timer      (repeatedTimer)
import Control.Monad                 (forM_)
import Control.Monad.Trans           (liftIO)
import Data.Aeson hiding (json)
import Data.IORef                    (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map as M                 (toList)
import Data.String                   (fromString)
import GHC.Generics                  (Generic)
import GHC.Int
import Lucid
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)
import Network.Wai                   (Middleware)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Room                          (cursorPosition, temperature)
import Simulation                    (Name, Simulation (..), addSituationForName, apply, applyAll, changeSituation, viewForName, newSimulation)
import Situation                     (evolve, halt, room, reset, start, state)
import Web.Spock                     (SpockM, SpockAction, json, jsonBody', get, getState, middleware, param', post, redirect, root, setStatus, spock, var, (<//>))
import Web.Spock.Config              (defaultSpockCfg, PoolOrConn(PCNoDatabase))
import Web.Spock.Lucid               (lucid)

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
    let simulation' = simulation >>= applyAll evolve
    _ <- atomicModifyIORef' ref $ const (simulation',simulation')
    return ()

app :: GHC.Int.Int64 -> IO Middleware
app delay = do
    ref <- newIORef (return newSimulation)
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
    spock spockCfg (routes delay)


routes :: GHC.Int.Int64 -> Api
routes delay = do
    let refresh = fromString $ show delay
    middleware (staticPolicy (addBase "static"))
    get root $ do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        lucid $ do
            head_ $ do
                link_ [ rel_ "stylesheet"
                      , type_ "text/css"
                      , href_ "/css/main.css"
                      ]
                meta_ [httpEquiv_ "Refresh", content_ refresh]
            h1_ "Simulations"
            let (Right (Simulation sits)) = simulation
            h2_ "situations"
            div_ [class_ "simulations"] $ do
                forM_ (M.toList sits) $ \sit -> 
                    div_ [class_ "simulation"] $ do
                        div_ [class_ "name"] $ do
                            toHtml $ (fst sit)
                        div_ [class_ "temperature"] $ do
                            toHtml $ (show (temperature (room (snd sit))))
                        div_ [class_ "cursor_position"] $ do
                            toHtml $ (show (cursorPosition (room (snd sit))))
                        div_ [class_ "simulation_state"] $ do
                            toHtml $ (show (state (snd sit)))
            h2_ "new situation"
            form_ [method_ "post", class_ "situation_input"] $ do
                div_ [class_ "situation_input"] $ do
                    label_ "Name: "
                    input_ [name_ "name"]
                div_ [class_ "situation_input"] $ do
                    input_ [type_ "submit", value_ "Create Situation"]

    post root $ do
        name <- param' "name"
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= addSituationForName name
        _ <- case newSim of
            Left s -> return s
            Right _ -> do
                _ <- liftIO $ atomicModifyIORef' ref $ const (newSim, newSim)
                return $ "Situation for " ++ name ++ " created."
        redirect $  "/"

    get "situations" $ do
        (AppState ref) <- Web.Spock.getState
        simulation <- liftIO $ readIORef ref
        json $ simulation

    get ("situations" <//> var) $ \name -> do
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= viewForName name
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
                json $ result >>= viewForName name

    post "situations" $ do
        name <- jsonBody' :: ApiAction Name
        (AppState ref) <- Web.Spock.getState
        sim <- liftIO $ readIORef ref
        let newSim = sim >>= addSituationForName name
        case newSim of
            Left _ -> do
                setStatus status204
                json $ newSim
            Right _ -> do
                result <- liftIO $ atomicModifyIORef' ref $ const (newSim, newSim)
                setStatus status201
                json $ result >>= viewForName name

