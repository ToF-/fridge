{-# LANGUAGE DeriveGeneric #-}
module Simulation (Simulation, Name, addSituation, apply, applyAll, changeSituation, getState, newSimulation)
    where

import Room
import Situation
import Data.Maybe (fromJust)
import Data.Map as M
import GHC.Generics
import Data.Aeson


type Name = String
data Simulation = Simulation (Map Name Situation)
    deriving (Generic, Eq, Show)

instance ToJSON Simulation

newSimulation :: Simulation
newSimulation = Simulation (M.empty)

getState :: Name -> Simulation -> Either String (State, Temperature, CursorPosition)
getState name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in Right (state sit, temperature (room sit), cursorPosition (room sit)))

addSituation :: Name -> Simulation -> Either String Simulation
addSituation name (Simulation ss) = case M.lookup name ss of
                                      Nothing -> Right (Simulation (M.insert name newSituation ss))
                                      Just _ -> Left ("a situation already exists with name:" ++ name)

checkName :: Name -> Simulation -> Either String Simulation
checkName name (Simulation ss) = case M.lookup name ss of
                                   Nothing -> Left ("no situation exists with name:" ++ name)
                                   Just _ -> Right (Simulation ss)

changeSituation :: Name -> CursorPosition -> Simulation -> Either String Simulation
changeSituation name pos s = return s >>= checkName name
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in case state sit of
                                                                      Started -> Right (Simulation (M.adjust (change pos) name ss))
                                                                      Halted -> Left ("situation for " ++ name ++ " is not started"))

apply :: (Situation -> Situation) -> Name -> Simulation -> Either String Simulation
apply f name s = return s >>= checkName name
    >>= (\(Simulation ss) -> Right (Simulation (M.adjust f name ss)))

applyAll :: (Situation -> Situation) -> Simulation -> Either String Simulation
applyAll f (Simulation ss) = return (Simulation (M.map f ss))

