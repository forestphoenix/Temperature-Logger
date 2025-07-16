{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Relude

import           Lib
import           Mqtt

-- | Main entry point
main :: IO ()
main = do
    -- Start MQTT client in the background
    runMqttClient "podman-freighter.lars-lan" 1883
    
    -- Start HTTP server
    runServer
