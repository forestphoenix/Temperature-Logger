{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Overview (
    latestMeasurementsToHtml
) where

import Relude

import           Measurements

import           Data.Time                   (LocalTime)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601    (iso8601Show)
import           Relude.Extra.Map as M
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)


-- | Convert latest measurements to an HTML table using Blaze HTML
latestMeasurementsToHtml :: LatestMeasurements -> Html
latestMeasurementsToHtml (LatestMeasurements dataByDevice) = do
    H.style $ toHtml $ unlines [
        ".measurements-table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }",
        ".measurements-table th { border: 1px solid #ddd; padding: 8px; text-align: left; background-color: #f2f2f2; }",
        ".measurements-table td { border: 1px solid #ddd; padding: 8px; }",
        ".measurements-table tr { border-bottom: 1px solid #ddd; }"
        ]
    H.table ! A.class_ "measurements-table" $ do
        H.thead $ do
            H.tr $ do
                H.th $ "Device"
                H.th $ "Temperature (°C)"
                H.th $ "Humidity (%)"
                H.th $ "Timestamp"
        H.tbody $ do
            forM_ (M.toPairs dataByDevice) $ \(deviceName, readout) -> do
                H.tr $ do
                    H.td $ toHtml deviceName
                    H.td $ toHtml (formatTemperature $ mrTemperature readout)
                    H.td $ toHtml (formatHumidity $ mrHumidity readout)
                    H.td $ toHtml (formatTimestamp $ mrTakenAt readout)

-- | Format temperature to 1 decimal place
formatTemperature :: Double -> String
formatTemperature = printf "%.1f"

-- | Format humidity to 1 decimal place
formatHumidity :: Double -> String
formatHumidity = printf "%.1f"

-- | Format timestamp in a human-readable format
formatTimestamp :: LocalTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
