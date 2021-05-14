{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where
import Control.Concurrent.Suspend    (sDelay)
import Control.Concurrent.Timer      (repeatedTimer)
import Network.Wai                   (Middleware)
import Data.String                   (fromString)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import GHC.Int                       (Int64)
import Web.Spock                     (SpockM, middleware, respondMiddleware, spock, get, root, (<//>), var, getState, json, setStatus, post, param', redirect, text)
import Web.Spock.Config              (defaultSpockCfg, PoolOrConn(PCNoDatabase))
import Web.Spock.Lucid               (lucid)
import Lucid                         (div_, class_,  h1_, body_, input_, name_, content_, head_, rel_, href_, link_, meta_, type_, httpEquiv_, value_, toHtml, br_, h2_, min_, max_, id_, span_, p_, script_, src_, oninput_, onsubmit_, form_, method_, action_, onclick_, onload_)

import Data.Text                     (Text)
import Control.Monad.IO.Class        (liftIO)
import Data.IORef                    (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Aeson                    (ToJSON, toJSON, object, (.=))
import Data.Map as M
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)

data Room = Room { temperature :: Double,
                   command :: Int }

instance ToJSON Room where
  toJSON r = object [
    "temperature" .= temperature r,
    "command"  .= command r]
data AppState = AppState (IORef (Map String Room))
type Server = SpockM () () AppState ()

type Delay = GHC.Int.Int64

repeatedAction :: IORef (Map String Room) -> IO ()
repeatedAction ref = do
    putStrLn "updating all rooms"
    rooms <- readIORef ref
    atomicModifyIORef' ref $ \rooms -> 
        (M.map (\room -> room { temperature = (temperature room) - 0.1 }) rooms, ())

app :: Delay -> IO Middleware
app delay = do
    ref <- newIORef (M.empty)
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
    spock spockCfg (routes delay)

routes :: Delay -> Server
routes delay = do
    let refresh = fromString $ show delay
    middleware (staticPolicy (addBase "static"))
    get root $ do
        liftIO $ putStrLn "GET /"
        lucid $ do
            head_ $ do
                link_ [ rel_ "stylesheet"
                      , type_ "text/css"
                      , href_ "/css/main.css"
                      ]
            body_ $ do
                div_ [class_ "center-div"] $ do
                    h1_ "Welcome to the fridge game!"
                    form_ [method_ "post"] $ do
                        input_ [id_ "room_name", type_ "text", name_ "name"]
                        input_ [type_ "submit", name_ "start", value_ "Start"]
                script_ [src_ "js/tools.js"] ("" :: String)
    get (root <//> var) $ \name -> do
        liftIO $ putStrLn ("GET /" <> fromString name)
        (AppState ref) <- Web.Spock.getState
        rooms <- liftIO $ readIORef ref
        let room = M.lookup name rooms
        case room of
          Nothing -> setStatus status400
          Just r -> do
              let temp = show (temperature r)
              let cmd  = show (command r)
              lucid $ do
                head_ $ do
                    link_ [ rel_ "stylesheet"
                          , type_ "text/css"
                          , href_ "/css/main.css"
                          ]
                body_ [onload_ "setRepeatedRefresh()"] $ do
                    div_ [class_ "center-div"] $ do
                        (p_ [id_ "name"] (fromString name))
                        (p_ [id_ "temperature"] (fromString temp))
                        (p_ [id_ "command"] (fromString cmd))
                        input_ [type_ "submit", name_ "refresh", value_ "Refresh", onclick_ "getTemperature()"]
                        form_ [action_ ("rooms/" <> fromString name), method_ "post"] $ do
                            input_ [type_ "range", name_ "command", min_ "1", max_ "200", value_ (fromString cmd), class_ "slider", id_ "command_range"]
                            input_ [type_ "submit", name_ "confirm", value_ "Confirm"]
                    script_ [src_ "js/slider.js"] ("" :: String)
                    script_ [src_ "js/refreshtemp.js"] ("" :: String)


    get ("rooms" <//> var) $ \name -> do
        liftIO $ putStrLn ("GET rooms/" <> fromString name)
        (AppState ref) <- Web.Spock.getState
        rooms <- liftIO $ readIORef ref
        let room = M.lookup name rooms
        case room of
          Just r -> do
              setStatus status200
              json room
          Nothing -> setStatus status204

    post root $ do
        liftIO $ putStrLn "POST /"
        name <- param' "name"
        (AppState ref) <- Web.Spock.getState
        let room = Room 15.0 100
        rooms <- liftIO $ readIORef ref
        liftIO $ atomicModifyIORef' ref $ \rooms -> (M.insert name room rooms, ())
        liftIO $ putStrLn ("creating room for " <> name)
        setStatus status202
        redirect ("/" <> fromString name)

    post ("rooms" <//> var) $ \name -> do
        liftIO $ putStrLn ("POST rooms/" <> fromString name)
        command <- param' "command"
        let cmd = read command
        (AppState ref) <- Web.Spock.getState
        rooms <- liftIO $ readIORef ref
        let room = M.lookup name rooms
        case room of
          Just r -> do
              setStatus status201
              liftIO $ atomicModifyIORef' ref $ \rooms -> (M.adjust (\room -> room { command = cmd }) name rooms, ())
              liftIO $ putStrLn ("updating room for " <> name <> " with command = " <> (show cmd))
              redirect ("/" <> fromString name)
          Nothing -> setStatus status204


