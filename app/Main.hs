module Main where

import Server                 (app)
import System.Environment  (getArgs)
import Web.Spock           (runSpock)

main :: IO ()
main = do
    args <- getArgs
    let refresh = case args of
              [] -> 60
              (s:_) -> read s
    putStrLn $ "refresh every " ++ show refresh ++ " seconds"
    runSpock 8080 (app refresh)
