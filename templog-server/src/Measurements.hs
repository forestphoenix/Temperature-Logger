{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Measurements (
    NewMeasurement(..),
    takeMeasurements,
    readLatestMeasurements,
) where

import           Data.Time                                (LocalTime)
import           Data.Time.Format.ISO8601                 (iso8601Show)
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
    convertMeasurements <$> runQuery' conn (relationalQuery latestMeasurements) ()

convertMeasurements :: [Measurement] -> LatestMeasurements
convertMeasurements measurements =
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
