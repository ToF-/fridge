module Room where

type Room = (Temperature, Position)
type RoomState = (Temperature, Position) 
type Temperature = Double
type Position = Integer

initialTemperature :: Temperature
initialTemperature = 15.0

initialPosition :: Position
initialPosition = 100

initialRoom :: Room
initialRoom = (initialTemperature, initialPosition)

room :: Temperature -> Position -> Room
room t p = (t,p)

temperature :: Room -> Temperature
temperature = fst

position :: Room -> Position
position = snd

state :: Room -> RoomState 
state r = (rounded (temperature r), position r)
    where
    rounded = (/ 10.0) . fromIntegral . round . (* 10) 
