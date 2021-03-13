module Room (Room, cursorPosition, newRoom, temperature)
    where

type Temperature = Double
type CursorPosition = Int
data Room = Nil


newRoom :: Room
newRoom = Nil

temperature :: Room -> Temperature
temperature = const 15.0 

cursorPosition :: Room -> CursorPosition
cursorPosition = const 100
