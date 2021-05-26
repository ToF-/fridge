{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Server
    where

import Control.Monad.IO.Class            (liftIO)
import Network.HTTP.Types.Status         (internalServerError500)
import IORepositoryRef                   (IORepositoryRef
                                         ,create
                                         ,update
                                         ,retrieve
                                         )
import Yesod.Form.Jquery                 (YesodJquery)
import Yesod                             (Yesod
                                         ,Html
                                         ,Value
                                         ,MForm
                                         ,FormResult (..)
                                         ,FormMessage
                                         ,RenderMessage
                                         ,defaultFormMessage
                                         ,returnJson
                                         ,notFound
                                         ,getYesod
                                         ,renderMessage
                                         ,julius
                                         ,areq
                                         ,sendResponseStatus
                                         ,requireJsonBody
                                         ,toWidget
                                         ,defaultLayout
                                         ,generateFormPost
                                         ,mkYesod
                                         ,parseRoutes
                                         ,redirect
                                         ,renderDivs
                                         ,renderRoute
                                         ,runFormPost
                                         ,textField
                                         ,warp
                                         ,whamlet
                                         )
import Data.ByteString.Char8 as BS       (putStrLn)
import Data.Text                         (Text
                                         ,unpack)
import Data.Text.Encoding                (encodeUtf8)
import Text.Julius                       (JavascriptUrl)
import Simulation                        (Simulation
                                         ,room
                                         ,roomView
                                         ,name)
import RoomView                          (RoomView (..))
import Repository                        (change)
import GraphHtml                         (graphHtml)
import Room                              (RoomState (..)
                                         ,state)

data FridgeApp = FridgeApp IORepositoryRef

data RoomName = RoomName { name :: Text }
    deriving Show

mkYesod "FridgeApp" [parseRoutes|
/                 HomeR GET
/room/#Text       RoomR GET
/newroom/         NewRoomR POST
/state/#Text      StateR GET PUT
|]

instance Yesod FridgeApp

instance YesodJquery FridgeApp

instance RenderMessage FridgeApp FormMessage where
    renderMessage _ _ = defaultFormMessage

roomNameForm :: Html -> MForm Handler (FormResult RoomName, Widget)
roomNameForm = renderDivs $ RoomName
    <$> areq textField "Name" Nothing

getHomeR :: Handler Html
getHomeR = do
    (formWidget, enctype) <- generateFormPost roomNameForm
    defaultLayout
        [whamlet|
            <p> Welcome to the fridge game!
            <p> Please enter your name
            <form method=post action=@{NewRoomR} enctype=#{enctype}>
                ^{formWidget}
                <button>Start
            |]

getStateR :: Text -> Handler Value
getStateR name = do
    (FridgeApp ref) <- getYesod
    found <- liftIO $ retrieve (unpack name) ref
    case found of
      Just sim -> returnJson (Simulation.roomView sim)
      Nothing -> notFound

putStateR :: Text -> Handler Value
putStateR name = do
    let nameAsString = unpack name
    position <- requireJsonBody :: Handler Int
    (FridgeApp ref) <- getYesod
    found <- liftIO $ retrieve nameAsString ref
    case found of
      Just sim -> do
          liftIO $ update  (change nameAsString position) ref
          liftIO $ BS.putStrLn ("changed position for " <> (encodeUtf8 name))
          returnJson (Simulation.roomView sim)
      Nothing -> notFound


postNewRoomR :: Handler Html
postNewRoomR = do
    ((result, formWidget), enctype) <- runFormPost roomNameForm
    case result of
        FormSuccess (RoomName name) -> do
            let nameAsString = unpack name
            (FridgeApp ref) <- getYesod
            found <- liftIO $ retrieve nameAsString ref
            case found of
                Just _ -> do
                    defaultLayout
                        [whamlet|
                            <p> A room with name #{name} already exists.
                            <form method=get action=@{HomeR} enctype=#{enctype}>
                                <button>Got it
                        |]
                Nothing -> do
                    liftIO $ create nameAsString ref
                    liftIO $ BS.putStrLn ("created room for name " <> (encodeUtf8 name))
                    redirect (RoomR name)
        FormFailure (m) -> do
            liftIO $ print m
            defaultLayout
                [whamlet|
                    $forall s <- m
                    <p> s
                    |]

sliderScript :: Text.Julius.JavascriptUrl url
sliderScript = [julius|
    var slider = document.getElementById("range");
    var output = document.getElementById("position");
    output.innerHTML = slider.value;
    var currentValue = output.innerHTML;
    slider.oninput = function() {
          output.innerHTML = this.value;
    }
    |]

refreshScript :: Text.Julius.JavascriptUrl url
refreshScript = [julius|
    async function putPosition() {
        if (currentValue != output.innerHTML) {
           let nameElement = document.getElementById("name");
           let name = nameElement.innerHTML;
           let posElement = document.getElementById("position");
           let pos = posElement.innerHTML;
           fetch('/state/'+name, {
            method: 'put',
            body: pos
           }).then(function(response) {
            return response.json();
          }).then(function(data) {
            console.log('put position');
         });
         currentValue = output.innerHTML;
         }
     }
    async function getTemperature() {
        let nameElement = document.getElementById("name");
        let name = nameElement.innerHTML;
        let tempElement = document.getElementById("temperature");
        fetch('/state/'+name)
        .then(function(response) {
            if (response.status !== 200) {
                console.log('error fetching ' + 'temperature/' + name + ' status: ' + response.status);
                return;
            }
            response.json().then(function(room) {
                console.log(room);
                tempElement.innerHTML = room.temperature;
            });
        }
        )
        .catch(function(err) {
            console.log('Fetch Error :-S', err);
        });

    }
    function setRepeatedRefresh() {
        window.setInterval(getTemperature, 30000);
        window.setInterval(putPosition, 1000);
    }
|]


openRoomPage :: Simulation -> Handler Html
openRoomPage sim = do
    let roomName = Simulation.name sim
    let view = roomView sim
    let temp = show (temperature view)
    let pos  = show (position view)
    defaultLayout $ do
        [whamlet|
            <body onload=setRepeatedRefresh()>
                <p>Room:
                <div id="name">#{roomName}
                <p>Temperature:
                <div id="temperature">#{temp}
                <p>Position:
                <div id="position">#{pos}
                <input id="range" type="range" min="1" max="200" value=#{pos}>
        |]
        toWidget sliderScript
        toWidget refreshScript

closedRoomPage :: Simulation -> Handler Html
closedRoomPage sim = defaultLayout (toWidget (graphHtml sim))

getRoomR :: Text -> Handler Html
getRoomR name = do
    (FridgeApp ref) <- getYesod
    let nameAsString = unpack name
    found <- liftIO $ retrieve nameAsString ref
    case found of
        Nothing -> do
          defaultLayout
            [whamlet|
                <p>There is no room with name #{name}
                <form method=get action=@{HomeR}>
                    <button>Got it
                    |]
        Just sim -> if state (room (sim)) == Open then openRoomPage sim else closedRoomPage sim

serve :: IORepositoryRef -> IO ()
serve ref = do
    warp 3000 (FridgeApp ref)

