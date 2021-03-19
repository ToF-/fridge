{-# LANGUAGE DeriveGeneric #-}
module Situation (Situation
                 , State (..)
                 , change
                 , halt
                 , history
                 , newSituation
                 , reset
                 , room
                 , state
                 , start
                 , tick
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

tick :: Situation -> Situation
tick situation | state situation == Started 
    = situation { rooms = (evolve (room situation)) : (rooms situation) }
               | otherwise = situation

halt :: Situation -> Situation
halt situation = situation { state = Halted }

reset :: Situation -> Situation
reset = const newSituation

change :: CursorPosition -> Situation -> Situation
change _ sit | state sit == Halted = sit
change pos sit = sit { rooms = (room' : rooms') }
    where
        room' = (head (rooms sit)) { cursorPosition = pos }
        rooms'= tail (rooms sit)

view :: Situation -> SituationView
view s = (st, temp, curspos)
    where
        st = state s
        temp = rounded (temperature r)
        curspos = cursorPosition r
        r = room s
        rounded :: Temperature -> Temperature
        rounded x = fromInteger (round (x * 10.0)) / 10.0
