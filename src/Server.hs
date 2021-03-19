module Server

    where

import Network.HTTP.Types.Status
import Simulation

data Server = Server { status :: Status,
                       message :: String,
                       simulation :: Simulation,
                       simulation_state :: Maybe SimulationState }

newServer :: Server
newServer = Server status200 "" newSimulation Nothing

getSituation :: Name -> Server -> Server
getSituation name server = 
    let sim = simulation server
        result = viewForName name sim
    in case result of 
         Right state -> Server status200 "" sim (Just state)
         Left msg ->    Server status204 msg sim Nothing

postSituation :: Name -> Server -> Server
postSituation name server = 
    let sim = simulation server
        result = addSituationForName name sim >>= viewForName name
    in case result of
         Right state -> Server status200 "" sim (Just state)
         Left msg -> Server status201 msg sim Nothing

