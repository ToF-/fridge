{-# LANGUAGE DeriveGeneric #-}
module Simulation ( Simulation (..)
                  , SimulationState
                  , Name
                  , add
                  , apply
                  , applyAll
                  , Simulation.change
                  , Simulation.new
                  , Simulation.view)
    where

import Control.FromSum (maybeToEither)
import Data.Aeson
import Data.Map as M
import GHC.Generics
import Room
import Situation


type Name = String
data Simulation = Simulation { situations :: Map Name Situation }
    deriving (Generic, Eq, Show)

type SimulationState = (State, Temperature, CursorPosition)
instance ToJSON Simulation

new :: Simulation
new = Simulation (M.empty)

view :: Name -> Simulation -> Either String SituationView
view name simulation = Situation.view <$> situation
    where
        situation = maybeToEither message (name ?? simulation)
        message   = "no situation exists with name:" ++ name

add :: Name -> Simulation -> Either String Simulation
add name simulation =
    case (name ?? simulation) of
      Nothing -> pure (name !> simulation)
      Just _  -> Left ("a situation already exists with name:" ++ name)

(??) :: Name -> Simulation -> Maybe Situation
name ?? simulation = M.lookup name (situations simulation)

(!>) :: Name -> Simulation -> Simulation
name !> simulation = Simulation situations'
    where situations' = M.insert name Situation.new (situations simulation)

nonExisting :: Name -> Either String a
nonExisting name = Left ("no situation exists with name:" ++ name)

checkName :: Name -> Simulation -> Either String Simulation
checkName name (Simulation ss) = case M.lookup name ss of
                                   Nothing -> nonExisting name
                                   Just _ -> Right (Simulation ss)

change :: CursorPosition -> Name -> Simulation -> Either String Simulation
change pos name simulation =
    case (name ?? simulation) of
      Nothing -> nonExisting name
      Just situation -> case state situation of
                          Started -> Right $ Simulation { situations = M.adjust (Situation.change pos) name (situations simulation) }
                          Halted -> Left ("situation for " ++ name ++ " is not started")

apply :: (Situation -> Situation) -> Name -> Simulation -> Either String Simulation
apply f name s = return s >>= checkName name
    >>= (\(Simulation ss) -> Right (Simulation (M.adjust f name ss)))

applyAll :: (Situation -> Situation) -> Simulation -> Either String Simulation
applyAll f (Simulation ss) = return (Simulation (M.map f ss))

