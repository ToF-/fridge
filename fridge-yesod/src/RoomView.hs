{-# LANGUAGE DeriveGeneric #-}
module RoomView
    where

import Room as R (Room, Temperature, Position, temperature, position)
import GHC.Generics
import Data.Aeson

data RoomView = RoomView {
    temperature :: Temperature,
    position :: Position }
    deriving (Generic, Eq, Show)

instance ToJSON RoomView

roomView :: Room -> RoomView
roomView room = RoomView
    { RoomView.temperature = rounded (R.temperature room),
      RoomView.position = R.position room }
    where
    rounded x = fromInteger (round (x * 10.0)) / 10.0
