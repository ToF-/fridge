module Simulation (Simulation, addSituation, getState, newSimulation)
    where

import Room
import Situation

type Name = String
data Simulation = Simulation [(Name, Situation)]

newSimulation :: Simulation
newSimulation = Simulation []

getState :: Simulation -> Name -> Either String (State, Temperature, CursorPosition)
getState (Simulation ss) name = case lookup name ss of
                      Nothing -> Left ("no situation exists with name:" ++ name)
                      Just sit -> Right (state sit, temperature (room sit), cursorPosition (room sit))

addSituation :: Simulation -> Name -> Simulation
addSituation (Simulation ss) name = Simulation ((name, newSituation) : ss)
