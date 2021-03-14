module Simulation (Simulation, addSituation, getState, newSimulation, startSituation)
    where

import Room
import Situation

type Name = String
data Simulation = Simulation [(Name, Situation)]
    deriving (Eq, Show)

newSimulation :: Simulation
newSimulation = Simulation []

getState :: Name -> Simulation -> Either String (State, Temperature, CursorPosition)
getState name (Simulation ss) = case lookup name ss of
                      Nothing -> Left ("no situation exists with name:" ++ name)
                      Just sit -> Right (state sit, temperature (room sit), cursorPosition (room sit))

addSituation :: Name -> Simulation -> Either String Simulation
addSituation name (Simulation ss) = case lookup name ss of
                                      Nothing -> Right (Simulation ((name, newSituation) : ss))
                                      Just _ -> Left ("a situation already exists with name:" ++ name)

startSituation :: Name -> Simulation -> Either String Simulation
startSituation name (Simulation ss) = case lookup name ss of
                                        Nothing -> Left ("no situation exists with name:" ++ name)
                                        Just sit -> Right (Simulation (update name (sit { state = Started }) ss))

update :: Name -> Situation -> [(Name, Situation)] -> [(Name, Situation)]
update _ _ [] = []
update target new ((name,sit):ss) | name == target = ((name, new) : ss) 
                                  | otherwise      = (name,sit): update target new ss
