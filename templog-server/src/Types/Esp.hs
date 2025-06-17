{-# LANGUAGE DeriveGeneric #-}

module Types.Esp (
    TemperatureReadout(..),
    CollectedReadings(..),
) where

import Relude
import           Data.Aeson (FromJSON) 

data TemperatureReadout = TemperatureReadout {
    device     :: Text,
    temp       :: Double,
    humidity   :: Double,
    deviceTime :: Int
} deriving (Generic, Show)
instance FromJSON TemperatureReadout

data CollectedReadings = CollectedReadings [TemperatureReadout]
    deriving (Generic, Show)
instance FromJSON CollectedReadings

-- JSON from ESP: [{"temp": 24.40, "humidity": 24.40, "deviceTime": 2, "device": "246f28c2d234"}, {"temp": 24.40, "humidity": 24.40, "deviceTime": 6, "device": "246f28c2d234"}, {"temp": 24.40, "humidity": 24.40, "deviceTime": 10, "device": "246f28c2d234"}, {"temp": 24.40, "humidity": 24.40, "deviceTime": 22, "device": "246f28c2d234"}, {"temp": 24.40, "humidity": 24.40, "deviceTime": 34, "device": "246f28c2d234"}, {"temp": 24.40, "humidity": 24.40, "deviceTime": 45, "device": "246f28c2d234"}]
