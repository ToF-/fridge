module Situation (Situation, State (..), newSituation, room, state)
    where

import Room

data Situation = Situation {
    room :: Room,
    state :: State }
    deriving (Eq, Show)

data State = Halted
    deriving (Eq, Show)

newSituation :: Situation
newSituation = Situation { room = newRoom, state = Halted }
