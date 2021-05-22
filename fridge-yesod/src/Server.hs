{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Server
    where
import           Yesod
import           Yesod.Form.Jquery
import           Text.Julius
import           Control.Applicative
import           Room
import           RoomView
import           Data.Text
import           IORepositoryRef
import           Repository
import           Simulation
import           Data.Aeson
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)

data FridgeApp = FridgeApp IORepositoryRef

mkYesod "FridgeApp" [parseRoutes|
/ HomeR GET
/simulation SimulationR POST
/room/#Int/#Text RoomR GET POST
/temperature/#Text TemperatureR GET
|]

instance Yesod FridgeApp

instance RenderMessage FridgeApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery FridgeApp

data SimulationName = SimulationName
    { name :: Text }
    deriving Show

data RoomPosition = RoomPosition
    { position :: Int }

simulationNameForm :: Html -> MForm Handler (FormResult SimulationName, Widget)
simulationNameForm = renderDivs $ SimulationName
    <$> areq textField "Name" Nothing

roomPositionForm :: Position -> Html -> MForm Handler (FormResult RoomPosition, Widget)
roomPositionForm position = renderDivs $ RoomPosition
    <$> areq intField "position" (Just position)

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost simulationNameForm
    defaultLayout
        [whamlet|
            <p> Welcome to the fridge game!
            <p> Please enter your name
                <form method=post action=@{SimulationR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
    |]

getRoomR :: Text -> Handler Html
getRoomR name = do
    (FridgeApp ref) <- getYesod
    found <- liftIO $ retrieve (unpack name) ref
    let sim = case found of
             Just s -> s
    let view = Simulation.roomView sim
    let position = RoomView.position view
    (widget, enctype) <- generateFormPost $ roomPositionForm position
    defaultLayout $ do
        toWidget [julius|
            async function getTemperature() {
                let nameElement = document.getElementById("name");
                let name = nameElement.innerHTML;
                let tempElement = document.getElementById("temperature");
                fetch('/temperature/'+name)
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
                window.setInterval(getTemperature, 3000);
            }
        |]
        [whamlet|
            <body onload=setRepeatedRefresh()>
            <p id="name">#{name}
            <p id="temperature">
            <form method=post action=@{RoomR name position} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

postRoomR :: Position -> Text -> Handler Html
postRoomR position name = do
    ((result, widget), enctype) <- runFormPost $ roomPositionForm position
    case result of
      FormSuccess (RoomPosition position) -> do
          liftIO $ putStrLn $ "changing position to " ++ show position ++ " for room " ++ (unpack name)
          (FridgeApp ref) <- getYesod
          liftIO $ IORepositoryRef.update (Repository.change (unpack name) position) ref
          redirect (RoomR name)
      _ -> defaultLayout [whamlet|
              <p>Invalid input, let'st try again.
              <form method=post action=@{RoomR name} enctype=#{enctype}>
                  ^{widget}
                  <imput type=submit>
                      |]

postSimulationR :: Handler Html
postSimulationR = do
    ((result, widget),enctype) <- runFormPost simulationNameForm
    case result of
        FormSuccess (SimulationName name) -> do
            (FridgeApp ref) <- getYesod
            liftIO $ create (unpack name) ref
            redirect (RoomR name)
        _ -> defaultLayout [whamlet|
            <p>Invalid input, let's try again.
                <form method=post action=@{SimulationR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>
                        |]

getTemperatureR :: Text -> Handler Value
getTemperatureR name = do
    (FridgeApp ref) <- getYesod
    found <- liftIO $ retrieve (unpack name) ref
    case found of
        Just sim -> returnJson (Simulation.roomView sim)
        Nothing -> notFound
serve :: IO ()
serve = do
    ref <- newIORepositoryRef
    warp 3000 (FridgeApp ref)

