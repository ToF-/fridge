{-# LANGUAGE DeriveGeneric #-}
module History ( History (..)
               , HistoryLine
               , newHistory
               , add
               , lastMinute)

    where
import Room (Room, Temperature, Position)
import RoomView
import GHC.Generics
import Data.Aeson

data History = History [HistoryLine]
    deriving (Generic, Eq, Show)
type HistoryLine = (Int, Temperature, Position)

instance ToJSON History

newHistory :: History
newHistory = History []

add :: Room -> History -> History
add room (History lines) = History (lines ++ [(minute, temp, pos)])
    where
        temp = temperature view
        pos  = position view
        view = roomView room
        minute = foldl (\acc _ -> acc + 1) 1 lines

lastMinute :: History -> Int
lastMinute (History lines) = length lines


