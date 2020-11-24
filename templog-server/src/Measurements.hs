{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE TemplateHaskell            #-}

module Measurements (
    NewMeasurement(..),
    takeMeasurement
) where

import           Data.Time                         (LocalTime)
import           Data.Time.Format.ISO8601          (iso8601Show)
import           Database.HDBC                     (IConnection, commit,
                                                    withTransaction)
import           Database.HDBC.Query.TH            (makeRelationalRecord)
import           Database.HDBC.Record
import           Database.Relational
import           Database.Relational.Documentation ((|$|), (|*|))

import           Db.Measurement

data NewMeasurement = NewMeasurement {
    device      :: String,
    temperature :: Double,
    humidity    :: Double,
    takenAt     :: LocalTime
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

takeMeasurement :: (IConnection conn) => conn -> NewMeasurement -> IO ()
takeMeasurement conn measurement = withTransaction conn $ \conn' -> do
    runInsert conn' insertNewMeasurement measurement
    commit conn'
