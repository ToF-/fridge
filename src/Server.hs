{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
import Lucid                         (Html, div_, class_,  h1_, body_, input_, name_, content_, head_, rel_, href_, link_, meta_, type_, httpEquiv_, value_, toHtml, br_, h2_, min_, max_, id_, span_, p_, script_, src_, oninput_, onsubmit_, form_, method_, action_, onclick_, onload_, table_, th_, tr_, td_)

import Data.Text                     (Text)
import Control.Monad.IO.Class        (liftIO)
import Data.IORef                    (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Aeson                    (ToJSON, toJSON, object, (.=))
import Data.Map as M
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)
import RoomState                     (RoomState (..))
import Room                          (Room, Temperature, CursorPosition, newRoom, evolve, change, open, close, state)
import RoomView                      (RoomView, roomView, temperature, command)
import History                       (History (..), add, newHistory, lastMinute)

data AppState = AppState (IORef (Map String (Room, History)))
type Server = SpockM () () AppState ()

type Delay = GHC.Int.Int64

duration :: Int
duration = 5

updateRoom :: (Room, History) -> (Room, History)
updateRoom (room, hist) | lastMinute hist < duration = (evolve room, add room hist)
                        | otherwise = (close room, hist)

repeatedAction :: IORef (Map String (Room, History)) -> IO ()
repeatedAction ref = do
    putStrLn "updating all rooms"
    atomicModifyIORef' ref $ \refState -> (M.map updateRoom refState, ())

app :: Delay -> IO Middleware
app delay = do
    ref <- newIORef (M.empty)
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    _ <- repeatedTimer (repeatedAction ref) (sDelay delay)
    spock spockCfg routes

pageHead :: Html ()
pageHead = head_ $ do
                link_ [ rel_ "stylesheet"
                      , type_ "text/css"
                      , href_ "/css/main.css"
                      ]
mainBody :: Html ()
mainBody = body_ $ do
                div_ [class_ "center-div"] $ do
                    h1_ "Welcome to the fridge game!"
                    form_ [method_ "post"] $ do
                        input_ [id_ "room_name", type_ "text", name_ "name"]
                        input_ [type_ "submit", name_ "start", value_ "Start"]
                script_ [src_ "js/tools.js"] ""

roomBody :: String -> RoomView -> Html ()
roomBody name view = do
    let temp = show (temperature view)
    let cmd  = show (command view)
    body_ [onload_ "setRepeatedRefresh()"] $ do
        div_ [class_ "center-div"] $ do
            (p_ [id_ "name"] (fromString name))
            (p_ [id_ "temperature"] (fromString temp))
            (p_ [id_ "command"] (fromString cmd))
            form_ [action_ ("rooms/" <> fromString name), method_ "post"] $ do
                input_ [type_ "range", name_ "command", min_ "0", max_ "200", value_ (fromString cmd), class_ "slider", id_ "command_range"]
                input_ [type_ "submit", name_ "confirm", value_ "Confirm"]
            script_ [src_ "js/slider.js"] ""
            script_ [src_ "js/refreshtemp.js"] ""

statLineHtml :: (Int, Temperature, CursorPosition) -> Html ()
statLineHtml (minute, temp, cmd) = do
    tr_ $ do
        td_ $ toHtml (show minute)
        td_ $ toHtml (show temp)
        td_ $ toHtml (show cmd)

statBody :: String -> History -> Html ()
statBody name (History lines) = do
    body_ $ do
        div_ [class_ "center-div"] $ do
            (p_ [id_ "name"] (fromString name))
            table_ $ do
                tr_ $ do
                    th_ "Minute"
                    th_ "Temperature"
                    th_ "Command"
                    mapM_ statLineHtml lines

routes :: Server
routes = do
    middleware (staticPolicy (addBase "static"))
    get root $ do
        liftIO $ putStrLn "GET /"
        lucid $ do
            pageHead
            mainBody

    get (root <//> var) $ \name -> do
        liftIO $ putStrLn ("GET /" <> fromString name)
        (AppState ref) <- Web.Spock.getState
        refState <- liftIO $ readIORef ref
        case M.lookup name refState of
          Nothing -> setStatus status400
          Just (room,hist) -> do
              let view = roomView room
              lucid $ do
                  pageHead
                  case state room of
                    Open -> roomBody name view
                    Closed -> statBody name hist

    get ("rooms" <//> var) $ \name -> do
        liftIO $ putStrLn ("GET rooms/" <> fromString name)
        (AppState ref) <- Web.Spock.getState
        refState <- liftIO $ readIORef ref
        case M.lookup name refState of
          Just (room,_) -> do
              setStatus status200
              json (roomView room)
          Nothing -> setStatus status204

    post root $ do
        liftIO $ putStrLn "POST /"
        name <- param' "name"
        (AppState ref) <- Web.Spock.getState
        refState <- liftIO $ readIORef ref
        liftIO $ atomicModifyIORef' ref $ \refState -> (M.insert name (open newRoom, newHistory) refState, ())
        liftIO $ putStrLn ("creating room for " <> name)
        setStatus status202
        redirect ("/" <> fromString name)

    post ("rooms" <//> var) $ \name -> do
        liftIO $ putStrLn ("POST rooms/" <> fromString name)
        command <- param' "command"
        let cmd = read command
        (AppState ref) <- Web.Spock.getState
        refState <- liftIO $ readIORef ref
        case M.lookup name refState of
          Just (room, hist) -> do
              setStatus status201
              liftIO $ atomicModifyIORef' ref $ \refState -> (M.adjust (\(room, hist) -> (change room cmd, hist)) name refState, ())
              liftIO $ putStrLn ("updating room for " <> name <> " with command = " <> (show cmd))
              redirect ("/" <> fromString name)
          Nothing -> setStatus status204


