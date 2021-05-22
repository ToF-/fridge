module IORepositoryRef
    where

import Data.IORef
import Repository
import Simulation

type IORepositoryRef = IORef Repository

newIORepositoryRef :: IO IORepositoryRef
newIORepositoryRef = newIORef newRepository

readIORepositoryRef :: IORepositoryRef -> IO Repository
readIORepositoryRef = readIORef

create :: String -> IORepositoryRef -> IO ()
create name ref = atomicModifyIORef' ref $
    \rep -> (Repository.add name rep, ())

retrieve :: String -> IORepositoryRef -> IO (Maybe Simulation)
retrieve name ref = do
    rep <- readIORepositoryRef ref
    return (findSimulation name rep)

update :: (Repository -> Repository) -> IORepositoryRef -> IO ()
update f ref = atomicModifyIORef' ref $
    \rep -> (f rep, ())
