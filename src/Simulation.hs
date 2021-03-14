module Simulation (Simulation, addSituation, changeSituation, getState, newSimulation, startSituation)
    where

import Room
import Situation
import Data.Maybe (fromJust)

type Name = String
data Simulation = Simulation [(Name, Situation)]
    deriving (Eq, Show)

newSimulation :: Simulation
newSimulation = Simulation []

getState :: Name -> Simulation -> Either String (State, Temperature, CursorPosition)
getState name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> let sit = fromJust (lookup name ss) in Right (state sit, temperature (room sit), cursorPosition (room sit)))

addSituation :: Name -> Simulation -> Either String Simulation
addSituation name (Simulation ss) = case lookup name ss of
                                      Nothing -> Right (Simulation ((name, newSituation) : ss))
                                      Just _ -> Left ("a situation already exists with name:" ++ name)

startSituation :: Name -> Simulation -> Either String Simulation
startSituation name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> Right (Simulation (update name start ss)))

update :: Name -> (Situation -> Situation) -> [(Name, Situation)] -> [(Name, Situation)]
update _ _ [] = []
update target f ((name,sit):ss) | name == target = (name, f sit) : ss
                                | otherwise      = (name,sit): update target f ss

checkName :: Name -> Simulation -> Either String Simulation
checkName name (Simulation ss) = case lookup name ss of
                                   Nothing -> Left ("no situation exists with name:" ++ name)
                                   Just _ -> Right (Simulation ss)

changeSituation :: Name -> CursorPosition -> Simulation -> Either String Simulation
changeSituation name pos s = return s >>= checkName name
    >>= (\(Simulation ss) -> let sit = fromJust (lookup name ss) in case state sit of
                                                                      Started -> Right (Simulation (update name (change pos) ss))
                                                                      Halted -> Left ("situation for " ++ name ++ " is not started"))
