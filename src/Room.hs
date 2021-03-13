module Room (Room, cursorPosition, newRoom, setCursorPosition, temperature)
    where

type Temperature = Double
type CursorPosition = Int
data Room = Room { 
    temperature :: Temperature,
    cursorPosition :: CursorPosition }
    deriving (Eq, Show)


newRoom :: Room
newRoom = Room 15.0 100

setCursorPosition :: Room -> CursorPosition -> Room
setCursorPosition room curPos = room { cursorPosition = curPos }
