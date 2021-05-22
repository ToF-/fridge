{-# LANGUAGE OverloadedStrings #-}
module Main where

import      Room (change, close, evolve, newRoom, open)
import      Data.Aeson (encode)
import      Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    let room = open newRoom
    BS.putStrLn $ "here is a new room (JSON): " <> encode room
    BS.putStrLn $ "here it is open: " <> (encode $ open room)
    BS.putStrLn $ "here it is evolving: " <> (encode $ evolve $ open room)
    BS.putStrLn $ "here it is changing: " <> (encode $ change 50 $ evolve $ open room)
    BS.putStrLn $ "here it is evolving again: " <> (encode $ evolve $ change 50 $ evolve $ open room)
    BS.putStrLn $ "here it is closed: " <> (encode $ close $ evolve $ change 50 $ evolve $ open room)
