{-# LANGUAGE DeriveGeneric #-}
module Simulation
    where

import GHC.Generics
import Room as R
import History
import RoomView as RV
import Data.Aeson

data Simulation = Simulation 
    { name :: String
    , room :: Room
    , history :: History }
  deriving (Generic, Eq, Show)

instance ToJSON Simulation
newSimulation :: String -> Simulation
newSimulation s = Simulation { name = s
                             , room = open newRoom
                             , history = newHistory }
roomView :: Simulation -> RoomView
roomView = RV.roomView . room

evolve :: Simulation -> Simulation
evolve sim | state (room (sim)) == Closed = sim
evolve sim | lastMinute (history sim) == 25 = sim { room = close (room sim) }
evolve sim = sim { room = room',
                   history = history' }
                       where
                           room' = R.evolve (room sim)
                           history' = add room' (history sim)

change :: Position -> Simulation -> Simulation
change pos sim = sim { room = R.change (room sim) pos }
