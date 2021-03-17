{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Situation
import           Simulation
import           Api
import           Web.Spock
import           Web.Spock.Config
import           Data.IORef

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Control.Concurrent.Suspend
import           Control.Concurrent.Timer
import           Network.Wai (Middleware)
import           Control.Monad.Trans (liftIO)


main :: IO ()
main = runSpock 8080 app

