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
                                         ,retrieve
                                         )
import Yesod.Form.Jquery                 (YesodJquery)
import Yesod                             (Yesod
                                         ,Html
                                         ,MForm
                                         ,FormResult (..)
                                         ,FormMessage
                                         ,RenderMessage
                                         ,defaultFormMessage
                                         ,getYesod
                                         ,renderMessage
                                         ,areq
                                         ,sendResponseStatus
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
                <button>Start
            |]

postStateR :: Handler Html
postStateR = do
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



getRoomR :: Text -> Handler Html
getRoomR name = do
    defaultLayout
        [whamlet|
            <p> room #{name}
            |]

serve :: IORepositoryRef -> IO ()
serve ref = do
    warp 3000 (FridgeApp ref)

