module Simulation where
import Room
import History

data Simulation = Simulation { 
    currentRoom :: Room,
    history :: History,
    status :: Status }
    deriving (Eq)

instance Show Simulation where
    show (Simulation room _ status) = show status 
                                      ++ " Temp=" ++ show (temperature room)
                                      ++ " Pos=" ++ show (position room)

data Status = Idle | Running
    deriving (Eq, Show)

type SimulationState = Either String Simulation

initial :: Simulation
initial = Simulation 
    (room 15.0 100)
    emptyHistory
    Idle 

newSimulation :: SimulationState
newSimulation = Right (Simulation (room 15.0 100) emptyHistory Idle)


update :: Simulation -> SimulationState
update s | status s == Idle = Right s
         | otherwise = Right (s { currentRoom = updatedCurrentRoom, history = updatedHistory })
    where 
    updatedCurrentRoom = room (currentTemperature + delta) currentPosition
        where
        delta = ((fromIntegral currentPosition) / 10.0 + 2.0 - temperatureAtTime (-4)) / 3.0
        currentTemperature  = temperature (currentRoom s)
        currentPosition     = position (currentRoom s)
        temperatureAtTime t = temperature ((history s) `at` t)
    updatedHistory = record (currentRoom s) (history s)

setPosition :: Position -> Simulation -> Either String Simulation
setPosition p s | status s == Running = case p `elem` [0..200] of
                                          False -> Left "INCORRECT POSITION"
                                          true -> Right $ s { currentRoom = (t,p) }
                                              where
                                                  (t,_) = currentRoom s 

setPosition _ _ = Left "SIMULATION NOT RUNNING"

start :: Simulation -> SimulationState
start s = Right $ s { status = Running }

stop :: Simulation -> SimulationState
stop s = Right $ s { status = Idle }
