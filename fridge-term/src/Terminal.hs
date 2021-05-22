module Terminal
    where

import IORepositoryRef
import Repository as R
import Simulation as S
import RoomView as V
import Command
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

execute :: Command -> IORepositoryRef -> IO ()
execute (Add name) ref = do
    create name ref
    putStrLn ("created room for name " <> name)

execute (Display name) ref = do
    found <- retrieve name ref
    withName name ref $ \sim -> printView sim

execute (Change name pos) ref = do
    found <- retrieve name ref
    withName name ref $ \sim -> do
        update (R.change name pos) ref
        putStrLn ("changed position for " <> name <> " to " <> (show pos))

execute Rooms ref = do
    rep <- readIORepositoryRef ref
    mapM_ (\sim -> do
        putStr (name sim <> " ")
        printView sim) (simulations rep)

execute (Command.Simulation name) ref = do
    withName name ref $ \sim -> do
        putStrLn (unpack (encode sim))

withName :: String -> IORepositoryRef -> (Simulation -> IO ()) -> IO ()
withName name ref action = do
    found <- retrieve name ref
    case found of
      Nothing -> putStrLn ("unknown name: " <> name)
      Just sim -> action sim

printView :: Simulation -> IO ()
printView sim = do
    let view = S.roomView sim
    putStrLn ("temperature: " <> (show (V.temperature view)) <> "   position: " <> (show (V.position view)))
