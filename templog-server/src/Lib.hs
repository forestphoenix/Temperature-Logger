{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runServer
    ) where

import           Data.Aeson            (FromJSON)
import           Web.Scotty            as WS

import           Database.HDBC         (IConnection)
import           Database.HDBC.Session (withConnectionIO)
import           Database.HDBC.Sqlite3

import           Data.Text             (unpack)
import           Data.Time

import           Measurements

data TemperatureFromESP = TemperatureFromESP {
    device   :: Text,
    temp     :: Double,
    humidity :: Double
} deriving (Generic, Show)

instance FromJSON TemperatureFromESP

runServer :: IO ()
runServer = scotty 5000 $ do
    WS.get "/" $ do
        current <- liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db3") $ \conn ->
            readLatestMeasurements conn
        text $ show current
    WS.post "/" $ do
        temperatureData <- jsonData
        liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db3") $ \conn ->
            recordTemperature conn temperatureData
        liftIO $ print $ "temperature taken: " <> show temperatureData
        text "Temperature recorded."

recordTemperature :: IConnection conn => conn -> TemperatureFromESP -> IO ()
recordTemperature conn (TemperatureFromESP device temp humidity) = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
    takeMeasurement conn $ NewMeasurement (unpack device) temp humidity localTime
