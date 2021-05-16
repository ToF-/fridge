module Repository
    where

import Simulation as S
import Data.Map as M

data Repository = Repository { map :: M.Map String Simulation }

newRepository = Repository M.empty

findSimulation :: String -> Repository -> Maybe Simulation
findSimulation name = (M.lookup name) . Repository.map

add :: String -> Repository -> Repository
add name r = r { Repository.map = M.insert name (newSimulation name) (Repository.map r) }

evolve :: Repository -> Repository
evolve r = r { Repository.map = M.map S.evolve (Repository.map r) }
