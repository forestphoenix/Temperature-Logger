{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mqtt
    ( runMqttClient
    ) where

import           Relude

import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Exception     (SomeException, catch)
import           Control.Monad         (forever)
import           Data.Aeson            (eitherDecode)
import qualified Data.ByteString.Lazy  as BL
import           Database.HDBC.Session (withConnectionIO)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Measurements          (recordTemperature)
import           Network.MQTT.Client
import           Network.MQTT.Types    (RetainHandling(..))
import           Network.URI           (parseURI)
import qualified Types.Esp             as Esp
import Network.MQTT.Topic (Topic(unTopic))

-- | Run the MQTT client to listen for temperature data
runMqttClient :: String -> Int -> IO ()
runMqttClient brokerHost port = do
    putStrLn $ "Starting MQTT client, connecting to " <> brokerHost <> ":" <> show port

    -- Start MQTT client in a separate thread
    _ <- forkIO $ forever $ do
        catch (connectAndSubscribe brokerHost port) $ \(e :: SomeException) -> do
            putStrLn $ "MQTT connection error: " <> show e
            threadDelay 5000000  -- Wait 5 seconds before reconnecting

    return ()

-- | Connect to MQTT broker and subscribe to topic
connectAndSubscribe :: String -> Int -> IO ()
connectAndSubscribe host port = do
    -- Create connection configuration
    let config = mqttConfig {
            _hostname = host,
            _port = port,
            _connID = "templog-client",
            _cleanSession = True,
            _lwt = Nothing,
            _msgCB = OrderedCallback handleMessage,
            _protocol = Protocol50,
            _connectTimeout = 5000000
        }
        (Just uri) = parseURI "mqtt://192.168.81.14"

    -- Connect to broker
    client <- connectURI config uri

    -- Subscribe to topic
    subscribe client [("templog/measurements", subOptions)] []

    -- Keep connection alive
    waitForClient client

    where
        subOptions = SubOptions DoNotSendOnSubscribe False False QoS1

-- | Handle incoming MQTT messages
handleMessage :: MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
handleMessage _ topic payload _ = do
    let topicStr = unTopic topic

    -- Only process messages from our topic
    when (topicStr == "templog/measurements") $ do
        -- Try to decode the payload as temperature readings
        case eitherDecode payload of
            Left err -> print $ "Failed to decode payload: " <> err <> "\n"
            Right readings -> do
                print $ "Successfully decoded temperature readings: " <> show readings <> "\n"
                -- Process the readings using the same function as the HTTP endpoint
                withConnectionIO (connectSqlite3 "db/database.db") $ \conn ->
                    recordTemperature conn readings
