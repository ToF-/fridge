{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module FridgePage where

import Yesod
data Fridge = Fridge
mkYesod "Fridge" [parseRoutes|
/ HomeR GET
|]

instance Yesod Fridge

getHomeR = defaultLayout $ do
    setTitle "Fridge"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    widgetGameName

widgetGameName = do
    toWidget [hamlet| <h1>Fridge |]
    toWidget [lucius| h1 { color:blue; }|]
