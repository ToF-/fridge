{-# LANGUAGE DeriveGeneric #-}
module RoomView
    where

import Room as R
import GHC.Generics
import Data.Aeson

data RoomView = RoomView {
    temperature :: Temperature,
    command :: CursorPosition }
    deriving (Generic, Eq, Show)

instance ToJSON RoomView

roomView :: Room -> RoomView
roomView room = RoomView
    { RoomView.temperature = rounded (R.temperature room),
      command = R.cursorPosition room }
    where
    rounded x = fromInteger (round (x * 10.0)) / 10.0
