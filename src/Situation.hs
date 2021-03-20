{-# LANGUAGE DeriveGeneric #-}
module Situation ( Situation
                 , SituationView
                 , State (..)
                 , change
                 , Situation.evolve
                 , halt
                 , history
                 , newSituation
                 , reset
                 , room
                 , start
                 , state
                 , view)
    where

import Data.Aeson
import GHC.Generics
import Room

data Situation = Situation {
    rooms :: [Room],
    state :: State }
    deriving (Generic, Eq, Show)

type SituationView = (State, Temperature, CursorPosition)

data State = Halted | Started
    deriving (Generic, Eq, Show)

instance ToJSON State
instance ToJSON Situation

room :: Situation -> Room
room = head . rooms

history :: Situation -> [Room]
history = reverse . rooms

newSituation :: Situation
newSituation = Situation { rooms = [newRoom], state = Halted }

start :: Situation -> Situation
start situation = situation { state = Started }

evolve :: Situation -> Situation
evolve situation | state situation == Started 
    = situation { rooms = (Room.evolve (room situation)) : (rooms situation) }
                 | otherwise = situation

halt :: Situation -> Situation
halt situation = situation { state = Halted }

reset :: Situation -> Situation
reset = const newSituation

change :: CursorPosition -> Situation -> Situation
change _ situation | state situation == Halted = situation
change pos situation = situation { rooms = (room' : rooms') }
    where
        room' = (room situation) { cursorPosition = pos }
        rooms'= tail (rooms situation)

view :: Situation -> SituationView
view situation = (st, temp, curspos)
    where
        st = state situation
        temp = rounded (temperature r)
        curspos = cursorPosition r
        r = room situation
        rounded :: Temperature -> Temperature
        rounded x = fromInteger (round (x * 10.0)) / 10.0
