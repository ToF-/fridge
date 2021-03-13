module Situation (Situation, State (..), halt, history, newSituation, room, state, start, tick)
    where

import Room

data Situation = Situation {
    rooms :: [Room],
    state :: State }
    deriving (Eq, Show)

data State = Halted | Started
    deriving (Eq, Show)

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


