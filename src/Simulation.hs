module Simulation (Simulation, addSituation, changeSituation, getState, newSimulation, startSituation, haltSituation)
    where

import Room
import Situation
import Data.Maybe (fromJust)
import Data.Map as M

type Name = String
data Simulation = Simulation (Map Name Situation)
    deriving (Eq, Show)

newSimulation :: Simulation
newSimulation = Simulation (M.empty)

getState :: Name -> Simulation -> Either String (State, Temperature, CursorPosition)
getState name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in Right (state sit, temperature (room sit), cursorPosition (room sit)))

addSituation :: Name -> Simulation -> Either String Simulation
addSituation name (Simulation ss) = case M.lookup name ss of
                                      Nothing -> Right (Simulation (M.insert name newSituation ss))
                                      Just _ -> Left ("a situation already exists with name:" ++ name)

startSituation :: Name -> Simulation -> Either String Simulation
startSituation name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> Right (Simulation (M.adjust start name ss)))

haltSituation :: Name -> Simulation -> Either String Simulation
haltSituation name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> Right (Simulation (M.adjust halt name ss)))

checkName :: Name -> Simulation -> Either String Simulation
checkName name (Simulation ss) = case M.lookup name ss of
                                   Nothing -> Left ("no situation exists with name:" ++ name)
                                   Just _ -> Right (Simulation ss)

changeSituation :: Name -> CursorPosition -> Simulation -> Either String Simulation
changeSituation name pos s = return s >>= checkName name
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in case state sit of
                                                                      Started -> Right (Simulation (M.adjust (change pos) name ss))
                                                                      Halted -> Left ("situation for " ++ name ++ " is not started"))
