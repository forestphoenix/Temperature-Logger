{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runServer
    ) where

import           Web.Scotty            as WS

import           Data.Semigroup        (Max (..), getMax)
import           Data.Text             (unpack)
import           Data.Time
import           Database.HDBC         (IConnection)
import           Database.HDBC.Session (withConnectionIO)
import           Database.HDBC.Sqlite3

import           Measurements
import qualified Types.Esp             as Esp

runServer :: IO ()
runServer = scotty 80 $ do
    WS.get "/" $ do
        current <- liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db3") $ \conn ->
            readLatestMeasurements conn
        text $ show current
    WS.post "/templog/v2/send"  $ do
        temperatureData <- jsonData
        liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db3") $ \conn ->
            recordTemperature conn temperatureData
        liftIO $ print $ "temperature taken: " <> show temperatureData
        text "Temperature recorded."

recordTemperature :: IConnection conn => conn -> Esp.CollectedReadings -> IO ()
recordTemperature conn readings = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
    takeMeasurements conn $ convertMeasurements readings localTime

convertMeasurements :: Esp.CollectedReadings -> LocalTime -> [NewMeasurement]
convertMeasurements (Esp.CollectedReadings rawReadings) timeReceived = convertSingle <$> rawReadings
    where
        convertSingle measurement = NewMeasurement
            (unpack $ Esp.device measurement)
            (Esp.temp measurement)
            (Esp.humidity measurement)
            (convertTime $ Esp.deviceTime measurement)
        convertTime deviceTime = addLocalTime
            (secondsToNominalDiffTime $ fromInteger $ toInteger $ deviceTime - lastTimeStamp) timeReceived
        lastTimeStamp = getMax $ mconcat $ Max <$> Esp.deviceTime <$> rawReadings
