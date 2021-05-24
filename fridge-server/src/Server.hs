{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Server
    where

import IORepositoryRef                   (IORepositoryRef)
import Yesod.Form.Jquery                 (YesodJquery)
import Yesod                             (Yesod
                                         ,Html
                                         ,MForm
                                         ,FormResult (..)
                                         ,FormMessage
                                         ,RenderMessage
                                         ,defaultFormMessage
                                         ,renderMessage
                                         ,areq
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

import Data.Text                         (Text)
data FridgeApp = FridgeApp IORepositoryRef

data RoomName = RoomName { name :: Text }
    deriving Show

mkYesod "FridgeApp" [parseRoutes|
/                 HomeR GET
/room/#Text       RoomR GET
/state/           StateR POST
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
            <p> Please entre your room name
            <form method=post action=@{StateR} enctype=#{enctype}>
                ^{formWidget}
                <button>Submit
            |]

postStateR :: Handler Html
postStateR = do
    ((result, formWidget), enctype) <- runFormPost roomNameForm
    case result of
      FormSuccess (RoomName name) -> do
          redirect (RoomR name)

getRoomR :: Text -> Handler Html
getRoomR name = do
    defaultLayout
        [whamlet|
            <p> room #{name}
            |]

serve :: IORepositoryRef -> IO ()
serve ref = do
    warp 3000 (FridgeApp ref)

