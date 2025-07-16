{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runServer
    ) where

import           Relude

import           Data.Text.Lazy                (Text)
import           Data.Time                     (ZonedTime)
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
runServer = scotty 8082 $ do
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
