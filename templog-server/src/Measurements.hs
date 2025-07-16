{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Measurements (
    NewMeasurement(..),
    LatestMeasurements(..),
    MeasurementReadout(..),
    takeMeasurements,
    readLatestMeasurements,
    recordTemperature,
    convertMeasurements,
) where

import Relude

import qualified Types.Esp
import           Data.Semigroup                           (Max (..), getMax)
import           Data.Text                                (unpack)
import           Data.Time                                (LocalTime,
                                                           ZonedTime (zonedTimeToLocalTime),
                                                           addLocalTime, getZonedTime,
                                                           secondsToNominalDiffTime)
import           Database.HDBC                            (IConnection, commit,
                                                           withTransaction)
import           Database.HDBC.Query.TH                   (makeRelationalRecord)
import           Database.HDBC.Record
import           Database.Record.Persistable              (PersistableWidth)
import           Database.Relational
import           Database.Relational.Documentation        ((|$|), (|*|))
import           Database.Relational.OverloadedInstances
import           Database.Relational.OverloadedProjection


import           Db.Measurement

data NewMeasurement = NewMeasurement {
    nmDevice      :: String,
    nmTemperature :: Double,
    nmHumidity    :: Double,
    nmTakenAt     :: LocalTime
} deriving (Generic)

$(makeRelationalRecord ''NewMeasurement)

insertNewMeasurement :: Insert NewMeasurement
insertNewMeasurement = insert piNewMeasurement

piNewMeasurement :: Pi Measurement NewMeasurement
piNewMeasurement =
    NewMeasurement |$| #device
                   |*| #temperature
                   |*| #humidity
                   |*| #takenAt

takeMeasurements :: (IConnection conn) => conn -> [NewMeasurement] -> IO ()
takeMeasurements conn measurements = withTransaction conn $ \conn' -> do
    forM_ measurements $ \measurement ->
        runInsert conn' insertNewMeasurement measurement
    commit conn'

data LatestMeasurements = LatestMeasurements {
    lmDataByDevice :: Map String MeasurementReadout
} deriving Show

data MeasurementReadout = MeasurementReadout {
    mrTemperature :: Double,
    mrHumidity    :: Double,
    mrTakenAt     :: LocalTime
} deriving Show

readLatestMeasurements :: (IConnection conn) => conn -> IO LatestMeasurements
readLatestMeasurements conn =
    convertDbMeasurements <$> runQuery' conn (relationalQuery latestMeasurements) ()

convertDbMeasurements :: [Measurement] -> LatestMeasurements
convertDbMeasurements measurements =
    LatestMeasurements $ fromList (convMeasurement <$> measurements)
        where
            convMeasurement m = (device m, MeasurementReadout {
                                                mrTemperature = temperature m,
                                                mrHumidity    = humidity m,
                                                mrTakenAt     = takenAt m })

latestMeasurements :: Relation () Measurement
latestMeasurements = relation $ do
    latestMeasurement <- query $ aggregateRelation $ do
        measured <- query measurement
        byDevice <- groupBy $ #device measured
        return $ byDevice >< max' (#measurementId measured)
    completeMeasurement <- query measurement
    wheres $ just (#measurementId completeMeasurement) .=. sndOfTuple latestMeasurement
    return completeMeasurement

sndOfTuple :: (PersistableWidth a, PersistableWidth b) => Record c (a, b) -> Record c b
sndOfTuple = decodeProjection

decodeProjection :: (PersistableWidth a, PersistableWidth b, HasProjection "snd" (a, b) b) => Record c (a, b) -> Record c b
decodeProjection r = #snd r

-- | Record temperature readings in the database
recordTemperature :: (IConnection conn) => conn -> Types.Esp.CollectedReadings -> IO ()
recordTemperature conn readings = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
    takeMeasurements conn $ convertMeasurements readings localTime

-- | Convert ESP temperature readings to database measurements
convertMeasurements :: Types.Esp.CollectedReadings -> LocalTime -> [NewMeasurement]
convertMeasurements (Types.Esp.CollectedReadings rawReadings) timeReceived = convertSingle <$> rawReadings
    where
        convertSingle measurement = NewMeasurement
            (unpack $ Types.Esp.device measurement)
            (Types.Esp.temp measurement)
            (Types.Esp.humidity measurement)
            (convertTime $ Types.Esp.deviceTime measurement)
        convertTime deviceTime = addLocalTime
            (secondsToNominalDiffTime $ fromInteger $ toInteger $ deviceTime - lastTimeStamp) timeReceived
        lastTimeStamp = getMax $ mconcat $ Max <$> Types.Esp.deviceTime <$> rawReadings
