module Game where

import Simulation
import Room
import Data.Map as M

type Game = Map PlayerId (Maybe Message, SimulationState)
data PlayerId = A | B | C | D | E | F
    deriving (Eq,Ord,Show,Read)
type Message = String
type Operation = Simulation -> SimulationState

newGame :: Game
newGame = M.empty

simulationFor :: PlayerId -> Game -> SimulationState
simulationFor p g = case M.lookup p g of
                      Just (m,s) -> s
                      Nothing -> Left ("NO EXISTING SIMULATION FOR: " ++ show p)

addPlayer :: PlayerId -> Game -> Game
addPlayer p g = M.insert p (Nothing, newSimulation) g


operationForPlayer :: Operation -> PlayerId -> Game -> Game
operationForPlayer o p g = let st = simulationFor p g in
                               case st >>= o of
                                 Left msg -> M.insert p (Just msg, st) g
                                 st'      -> M.insert p (Nothing, st') g

setPositionForPlayer :: Position -> PlayerId -> Game -> Game
setPositionForPlayer n = operationForPlayer (setPosition n)

startForPlayer :: PlayerId -> Game -> Game
startForPlayer = operationForPlayer start

stopForPlayer :: PlayerId -> Game -> Game
stopForPlayer = operationForPlayer stop

stateForPlayer :: PlayerId -> Game -> Either String RoomState
stateForPlayer p g = fmap (state . currentRoom) (simulationFor p g)

doAll :: Operation -> Game -> Game
doAll o = M.map (\(msg, st) -> (msg, st >>= o))

updateRunningSimulations :: Game -> Game
updateRunningSimulations = doAll Simulation.update

startAll :: Game -> Game
startAll = doAll start

stopAll :: Game -> Game
stopAll = doAll stop

messageForPlayer :: PlayerId -> Game -> Maybe String
messageForPlayer p g = case M.lookup p g of
                         Just (msg, s) -> msg
                         Nothing -> Just ("NO EXISTING SIMULATION FOR: " ++ show p) 

showAll :: Game -> [String]
showAll = Prelude.map (uncurry showPlayerSimulation) . M.assocs
    where
    showPlayerSimulation :: PlayerId -> (Maybe Message, SimulationState) -> String
    showPlayerSimulation playerId (_, Right simState) = show playerId ++ ": " ++ show simState

