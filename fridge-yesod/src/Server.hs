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
import           Control.Applicative
import           Data.Text
import           IORepositoryRef
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)

data FridgeApp = FridgeApp IORepositoryRef

mkYesod "FridgeApp" [parseRoutes|
/ HomeR GET
/simulation SimulationR POST
/rooms/#Text RoomsR GET
/room RoomR POST
|]

instance Yesod FridgeApp

instance RenderMessage FridgeApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery FridgeApp

data SimulationName = SimulationName
    { name :: Text }
    deriving Show

simulationNameForm :: Html -> MForm Handler (FormResult SimulationName, Widget)
simulationNameForm = renderDivs $ SimulationName
    <$> areq textField "Name" Nothing

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

getRoomsR :: Text -> Handler Html
getRoomsR name = defaultLayout $ do
    toWidget [lucius| h1 { color:darkblue; text-align: center; } |]
    toWidget [hamlet| <h1>#{name}|]
    toWidget [lucius| body { background-color: lightgrey; color: darkblue; font-family: verdana; font-size: 20px } |]

postSimulationR :: Handler Html
postSimulationR = do
    ((result, widget),enctype) <- runFormPost simulationNameForm
    case result of
        FormSuccess (SimulationName name) -> redirect (RoomsR name)
        _ -> defaultLayout [whamlet|
            <p>Invalid input,let's try again.
                <form method=post action=@{SimulationR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>
                        |]

postRoomR :: Handler ()
postRoomR = do
    name <- runInputGet (ireq textField "name")
    redirect $ RoomsR name

serve :: IO ()
serve = do
    ref <- newIORepositoryRef
    warp 3000 (FridgeApp ref)

