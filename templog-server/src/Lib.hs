{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runServer
    ) where

import           Relude

import           Data.Semigroup                (Max (..), getMax)
import           Data.Text                     (unpack)
import           Data.Text.Lazy                (Text)
import           Data.Time                     (LocalTime,
                                                ZonedTime (zonedTimeToLocalTime),
                                                addLocalTime, getZonedTime,
                                                secondsToNominalDiffTime)
import           Database.HDBC                 (IConnection, SqlError)
import           Database.HDBC.Session         (withConnectionIO)
import           Database.HDBC.Sqlite3
import           Measurements
import           Network.Wai                   (Response)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Types.Esp                     as Esp
import           Views.Overview
import           Web.Scotty                    as WS
import           Web.Scotty.Trans              (ErrorHandler)

handleSqlError :: MonadIO m => ErrorHandler m
handleSqlError = Handler $ \(err :: SqlError) -> do
    liftIO $ print $ "sql Error occurred: " ++ show err

runServer :: IO ()
runServer = scotty 8080 $ do
    defaultHandler handleSqlError

    WS.get "/test" $ text "running."
    WS.get "/" $ do
        current <- liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db") $ \conn ->
            readLatestMeasurements conn
        WS.html $ renderHtml $ latestMeasurementsToHtml current
    WS.get "/raw" $ do
        current <- liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db") $ \conn ->
            readLatestMeasurements conn
        text $ show current
    WS.post "/templog/v2/send"  $ do
        temperatureData <- jsonData
        liftAndCatchIO $ withConnectionIO (connectSqlite3 "db/database.db") $ \conn ->
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
