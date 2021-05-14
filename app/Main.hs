module Main where

import Server                 (app)
import System.Environment  (getArgs)
import Web.Spock           (runSpock)

main :: IO ()
main = do
    runSpock 8080 (app 6)
