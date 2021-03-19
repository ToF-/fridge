{-# LANGUAGE DeriveGeneric #-}
module Simulation ( Simulation (..)
                  , SimulationState
                  , Name
                  , addSituationForName
                  , apply
                  , applyAll
                  , changeSituation
                  , newSimulation
                  , viewForName)
    where

import Control.FromSum (maybeToEither)
import Data.Aeson
import Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics
import Room
import Situation


type Name = String
data Simulation = Simulation { situations :: Map Name Situation }
    deriving (Generic, Eq, Show)

type SimulationState = (State, Temperature, CursorPosition)
instance ToJSON Simulation

newSimulation :: Simulation
newSimulation = Simulation (M.empty)

viewForName :: Name -> Simulation -> Either String SituationView
viewForName name simulation = view <$> situation
    where
        situation = maybeToEither message (name ?? simulation)
        message   = "no situation exists with name:" ++ name

addSituationForName :: Name -> Simulation -> Either String Simulation
addSituationForName name simulation = 
    case (name ?? simulation) of
      Nothing -> pure (name !> simulation)
      Just _  -> Left ("a situation already exists with name:" ++ name)

(??) :: Name -> Simulation -> Maybe Situation
name ?? simulation = M.lookup name (situations simulation) 

(!>) :: Name -> Simulation -> Simulation
name !> simulation = Simulation situations'
    where situations' = M.insert name newSituation (situations simulation)

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

