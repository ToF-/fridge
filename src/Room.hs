{-# LANGUAGE DeriveGeneric #-}
module Room ( Room
            , Temperature
            , CursorPosition
            , change
            , cursorPosition
            , evolve
            , newRoom
            , temperature)
    where
import GHC.Generics
import Data.Aeson

type Temperature = Double
type CursorPosition = Int
data Room = Room {
    temperatures :: [Temperature],
    cursorPosition :: CursorPosition }
    deriving (Generic, Eq, Show)

instance ToJSON Room

newRoom :: Room
newRoom = Room [15.0, 15.0, 15.0, 15.0, 15.0] 100

change :: Room -> CursorPosition -> Room
change room curPos = room { cursorPosition = (min 200 (max 0 curPos)) }

temperature :: Room -> Temperature
temperature room = head (temperatures room)

evolve :: Room -> Room
evolve room = room { temperatures = temperatures' }
    where
        temperatures' = take 5 ((temperature room + delta) : temperatures room)
        delta = (fromIntegral (cursorPosition room)
                / 10.0 + 2.0 - (temperatures room) !! 4)
                / 3.0
