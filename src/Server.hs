{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where
import Network.Wai                   (Middleware)
import Data.String                   (fromString)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import GHC.Int                       (Int64)
import Web.Spock                     (SpockM, middleware, spock, get, root)
import Web.Spock.Config              (defaultSpockCfg, PoolOrConn(PCNoDatabase))
import Web.Spock.Lucid               (lucid)
import Lucid                         (div_, class_,  h1_, body_, input_, name_, content_, head_, rel_, href_, link_, meta_, type_, httpEquiv_, value_)

data AppState = Nil
type Server = SpockM () () AppState ()

app :: GHC.Int.Int64 -> IO Middleware
app delay = do
    spockCfg <- defaultSpockCfg () PCNoDatabase Nil
    spock spockCfg (routes delay)

routes :: GHC.Int.Int64 -> Server
routes delay = do
    let refresh = fromString $ show delay
    middleware (staticPolicy (addBase "static"))
    get root $ do
        lucid $ do
            head_ $ do
                link_ [ rel_ "stylesheet"
                      , type_ "text/css"
                      , href_ "/css/main.css"
                      ]
                meta_ [httpEquiv_ "Refresh", content_ refresh]
            body_ $ do
                div_ [class_ "center-div"] $ do
                    h1_ "Welcome to the fridge game!"
                    input_ [type_ "text", name_ "name"]
                    input_ [type_ "submit", name_ "start", value_ "Start"]
