module Repository
    where

import Simulation as S
import Room
import Data.Map as M

data Repository = Repository { map :: M.Map String Simulation }
    deriving (Eq, Show)

newRepository = Repository M.empty

findSimulation :: String -> Repository -> Maybe Simulation
findSimulation name = (M.lookup name) . Repository.map

add :: String -> Repository -> Repository
add name (Repository m) = Repository (M.insert name (newSimulation name) m)

evolve :: Repository -> Repository
evolve (Repository m) = Repository (M.map S.evolve m)

change :: String -> Position -> Repository -> Repository
change name pos (Repository m) = Repository (M.adjust (\sim -> S.change pos sim) name m)
