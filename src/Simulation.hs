{-# LANGUAGE DeriveGeneric #-}
module Simulation ( Simulation (..)
                  , SimulationState
                  , Name
                  , addSituationForName
                  , apply
                  , applyAll
                  , changeSituation
                  , newSimulation
                  , stateForName)
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

type SimulationState = (State, Temperature, CursorPosition)
instance ToJSON Simulation

newSimulation :: Simulation
newSimulation = Simulation (M.empty)

stateForName :: Name -> Simulation -> Either String SimulationState
stateForName name s = return s >>= checkName name 
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in Right (state sit, temperature (room sit), cursorPosition (room sit)))

addSituationForName :: Name -> Simulation -> Either String Simulation
addSituationForName name (Simulation ss) = case M.lookup name ss of
                                      Nothing -> Right (Simulation (M.insert name newSituation ss))
                                      Just _ -> Left ("a situation already exists with name:" ++ name)

checkName :: Name -> Simulation -> Either String Simulation
checkName name (Simulation ss) = case M.lookup name ss of
                                   Nothing -> Left ("no situation exists with name:" ++ name)
                                   Just _ -> Right (Simulation ss)

changeSituation :: CursorPosition -> Name -> Simulation -> Either String Simulation
changeSituation pos name s = return s >>= checkName name
    >>= (\(Simulation ss) -> let sit = fromJust (M.lookup name ss) in case state sit of
                                                                      Started -> Right (Simulation (M.adjust (change pos) name ss))
                                                                      Halted -> Left ("situation for " ++ name ++ " is not started"))

apply :: (Situation -> Situation) -> Name -> Simulation -> Either String Simulation
apply f name s = return s >>= checkName name
    >>= (\(Simulation ss) -> Right (Simulation (M.adjust f name ss)))

applyAll :: (Situation -> Situation) -> Simulation -> Either String Simulation
applyAll f (Simulation ss) = return (Simulation (M.map f ss))

