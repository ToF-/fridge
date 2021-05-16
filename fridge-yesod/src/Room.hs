{-# LANGUAGE DeriveGeneric #-}
module Room ( Room
            , Temperature
            , Position
            , RoomState (..)
            , change
            , close
            , position
            , evolve
            , newRoom
            , open
            , state
            , temperature)
    where
import GHC.Generics
import Data.Aeson

data RoomState = Closed | Open
    deriving (Generic, Eq, Show)

instance ToJSON RoomState

type Temperature = Double
type Position = Int
data Room = Room {
    state :: RoomState,
    temperatures :: (Temperature
                    ,Temperature
                    ,Temperature
                    ,Temperature
                    ,Temperature),
    position :: Position }
    deriving (Generic, Eq, Show)

instance ToJSON Room

newRoom :: Room
newRoom = Room {
    state = Closed,
    temperatures = (15.0, 15.0, 15.0, 15.0, 15.0),
    position = 100 }

change :: Room -> Position -> Room
change room _ | state room /= Open = room
change room curPos = room { position = (min 200 (max 0 curPos)) }

temperature :: Room -> Temperature
temperature room = temperature
    where
        (temperature,_,_,_,_) = temperatures room

evolve :: Room -> Room
evolve room | state room /= Open = room
evolve room = room { temperatures = temperatures' }
    where
        temperatures' = (t1 + delta, t1, t2, t3, t4)
        (t1,t2,t3,t4,t5) = temperatures room
        delta = (fromIntegral (position room)
                / 10.0 + 2.0 - t5)
                / 3.0

open :: Room -> Room
open room = room { state = Open }

close :: Room -> Room
close room = room { state = Closed }
