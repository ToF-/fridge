{-# LANGUAGE DeriveGeneric #-}
module Room ( Room
            , Temperature
            , CursorPosition
            , change
            , close
            , cursorPosition
            , evolve
            , newRoom
            , open
            , state
            , temperature)
    where

import RoomState (RoomState (..))
type Temperature = Double
type CursorPosition = Int
data Room = Room {
    state :: RoomState,
    temperatures :: (Temperature
                    ,Temperature
                    ,Temperature
                    ,Temperature
                    ,Temperature),
    cursorPosition :: CursorPosition }
    deriving (Eq, Show)

newRoom :: Room
newRoom = Room {
    state = Closed,
    temperatures = (15.0, 15.0, 15.0, 15.0, 15.0),
    cursorPosition = 100 }

change :: Room -> CursorPosition -> Room
change room _ | state room /= Open = room
change room curPos = room { cursorPosition = (min 200 (max 0 curPos)) }

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
        delta = (fromIntegral (cursorPosition room)
                / 10.0 + 2.0 - t5)
                / 3.0

open :: Room -> Room
open room = room { state = Open }

close :: Room -> Room
close room = room { state = Closed }
