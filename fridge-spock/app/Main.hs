module Main where

import Server                 (app)
import System.Environment  (getArgs)
import Web.Spock           (runSpock)

changeIntervalInSeconds = 6

main :: IO ()
main = runSpock 8080 (app changeIntervalInSeconds)
