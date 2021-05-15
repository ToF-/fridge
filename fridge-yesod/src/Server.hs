{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns      #-}
module Server
    where
import           Yesod
import           Data.Text
import Network.HTTP.Types.Status     (status200, status201, status202, status204, status400)

data FridgeApp = FridgeApp



mkYesod "FridgeApp" [parseRoutes|
/ HomeR GET
/rooms/#Text RoomsR GET
/room RoomR POST
|]

instance Yesod FridgeApp

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "The Fridge Game"
    toWidget [lucius| h1 { color:darkblue; text-align: center; } |]
    toWidget [hamlet| <h1>Welcome in the fridge game!|]
    toWidget [lucius| body { background-color: lightgrey; color: darkblue; font-family: verdana; font-size: 20px } |]
    [whamlet| <form action=@{RoomR} method=post>
              <input type=text name=name>
              <input type=submit value="Start"> |]

postRoomR :: Handler ()
postRoomR = do
    redirect $ RoomsR "ToF"

getRoomsR :: Text -> Handler RepHtml
getRoomsR name = defaultLayout $ do
    toWidget [lucius| h1 { color:darkblue; text-align: center; } |]
    toWidget [hamlet| <h1>#{name}|]
    toWidget [lucius| body { background-color: lightgrey; color: darkblue; font-family: verdana; font-size: 20px } |]

serve :: IO ()
serve = warp 3000 FridgeApp
