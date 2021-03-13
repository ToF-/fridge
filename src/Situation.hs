module Situation (Situation, newSituation, room)
    where

import Room

data Situation = Situation {
    room :: Room }
    deriving (Eq, Show)

newSituation :: Situation
newSituation = Situation { room = newRoom }
