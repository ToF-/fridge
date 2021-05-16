module Simulation
    where

import Room
import History
import RoomView as RV

data Simulation = Simulation 
    { name :: String
    , room :: Room
    , history :: History }
  deriving (Eq, Show)

newSimulation :: String -> Simulation
newSimulation s = Simulation { name = s
                             , room = newRoom
                             , history = newHistory }
roomView :: Simulation -> RoomView
roomView = RV.roomView . room
