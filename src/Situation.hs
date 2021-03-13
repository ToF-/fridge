module Situation (Situation, State (..), newSituation, room, state, start, tick)
    where

import Room

data Situation = Situation {
    room :: Room,
    state :: State }
    deriving (Eq, Show)

data State = Halted | Started
    deriving (Eq, Show)

newSituation :: Situation
newSituation = Situation { room = newRoom, state = Halted }

start :: Situation -> Situation
start situation = situation { state = Started }

tick :: Situation -> Situation
tick situation = situation { room = evolve (room situation) }
