{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Message(..),
    Config(..),
    AnvizConfig(..),
    SerialConfig(..),
    ServerConfig(..),
    ActionsConfig(..)
    ) where

import Data.ByteString hiding (putStrLn)
import Data.Yaml
import Control.Applicative
import GHC.Word

data Message = Request ByteString
             | Responce ByteString
             | Serial ByteString

data Config = Config    { anviz :: AnvizConfig
                        , serial :: SerialConfig
                        , server :: ServerConfig
                        , actions :: ActionsConfig
                        } deriving Show

instance FromJSON Config where
    parseJSON (Object m) = Config <$>
        m .: "anviz" <*>
        m .: "serial" <*>
        m .: "server" <*>
        m .: "actions"
    parseJSON x = fail ("not an object: " ++ show x) 

data AnvizConfig = AnvizConfig {
    anviz_ip :: String,
    anviz_port :: String,
    anviz_users :: [[Word8]]
} deriving Show

instance FromJSON AnvizConfig where
    parseJSON (Object m) = AnvizConfig <$>
        m .: "anviz_ip" <*>
        m .: "anviz_port" <*>
        m .: "anviz_users"
    parseJSON x = fail ("not an object: " ++ show x) 

data SerialConfig = SerialConfig {
    serial_port :: String,
    serial_users :: [Int]
} deriving Show

instance FromJSON SerialConfig where
    parseJSON (Object m) = SerialConfig <$>
        m .: "serial_port" <*>
        m .: "serial_users"
    parseJSON x = fail ("not an object: " ++ show x) 

data ServerConfig = ServerConfig {
    server_eth :: String,
    server_port :: String
} deriving Show

instance FromJSON ServerConfig where
    parseJSON (Object m) = ServerConfig <$>
        m .: "server_eth" <*>
        m .: "server_port"
    parseJSON x = fail ("not an object: " ++ show x) 

data ActionsConfig = ActionsConfig {
    run :: String
} deriving Show

instance FromJSON ActionsConfig where
    parseJSON (Object m) = ActionsConfig <$>
        m .: "run"
    parseJSON x = fail ("not an object: " ++ show x) 
