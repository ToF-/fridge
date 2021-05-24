{-# LANGUAGE DeriveGeneric #-}
module GraphData
    where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Simulation
import History

data GraphData = GraphData {
    _data ::  GraphContentData,
    _layer :: [GraphLayer] }
    deriving (Generic)

instance ToJSON GraphData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

data GraphContentData = GraphContentData {
    values :: [GraphValue] }
    deriving (Generic)

instance ToJSON GraphContentData

data GraphValue = GraphValue {
    minute :: Int,
    temperature :: Double,
    position :: Double }
    deriving (Generic)

instance ToJSON GraphValue

data GraphLayer = GraphLayer {
    _mark :: GraphMark,
    _encoding :: GraphEncoding }
    deriving (Generic)

instance ToJSON GraphLayer where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

data GraphMark = GraphMark {
    __type :: String,
    __point :: Bool }
    deriving (Generic)

instance ToJSON GraphMark where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

data GraphEncoding = GraphEncoding {
    x :: GraphField,
    y :: GraphField,
    color :: GraphColor }
    deriving (Generic)

instance ToJSON GraphEncoding

data GraphField = GraphField {
    _field :: String,
    _type  :: String }
    deriving (Generic)

instance ToJSON GraphField where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

data GraphColor = GraphColor {
    __value :: String }
    deriving (Generic)

instance ToJSON GraphColor where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

graphData :: Simulation -> GraphData
graphData sim = GraphData {
    _data = GraphContentData { values = map graphDataValue lines },
    _layer = [GraphLayer {
             _mark = GraphMark {
             __type = "line",
             __point = True } ,
             _encoding = GraphEncoding {
                x = GraphField {
                _field = "minute",
                _type = "quantitative" },
                y = GraphField {
                _field = "temperature",
                _type = "quantitative" },
                color = GraphColor { __value = "#dc3a17" }
                                       }
                         }
             ,GraphLayer {
             _mark = GraphMark {
             __type = "line",
             __point = True } ,
             _encoding = GraphEncoding {
                x = GraphField {
                _field = "minute",
                _type = "quantitative" },
                y = GraphField {
                _field = "position",
                _type = "quantitative" },
                color = GraphColor { __value ="#1792dc" }
                                       }
                         }
             ] }
    where
        (History lines) = history sim

graphDataValue :: HistoryLine -> GraphValue
graphDataValue (m,t,p) = GraphValue {
    minute = m,
    temperature = t,
    position = (fromIntegral p)/10 }
