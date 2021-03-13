module Room (Room, cursorPosition, newRoom, setCursorPosition, temperature, evolve)
    where

type Temperature = Double
type CursorPosition = Int
data Room = Room { 
    temperatures :: [Temperature],
    cursorPosition :: CursorPosition }
    deriving (Eq, Show)


newRoom :: Room
newRoom = Room [15.0, 15.0, 15.0, 15.0, 15.0] 100

setCursorPosition :: Room -> CursorPosition -> Room
setCursorPosition room curPos = room { cursorPosition = curPos }

temperature :: Room -> Temperature
temperature room = head (temperatures room)

evolve :: Room -> Room
evolve room = room { temperatures = temperatures' }
    where
        temperatures' = (temperature room + delta) : temperatures room
        delta = (fromIntegral (cursorPosition room) 
                / 10.0 + 2.0 - (temperatures room) !! 4) 
                / 3.0
