{-# LANGUAGE OverloadedStrings #-}
module              Lib                     ( Message(..)
                                            , Config(..)
                                            , AnvizConfig(..)
                                            , SerialConfig(..)
                                            , ServerConfig(..)
                                            , ActionsConfig(..)
                                            , LoggerConfig(..)
                                            , lcom
                                            ) where

import           Control.Applicative
import           Data.ByteString     as B
import           Data.Yaml
import           GHC.Word
import           System.Log.Logger

lcom = "Logger.Main"

data Message = Request ByteString
             | Responce ByteString
             | Serial String

data Config = Config    { anviz   :: AnvizConfig
                        , serial  :: SerialConfig
                        , server  :: ServerConfig
                        , actions :: ActionsConfig
                        , logger  :: LoggerConfig
                        } deriving Show

instance FromJSON Config where
  parseJSON (Object m) = Config <$>
    m .: "anviz" <*>
    m .: "serial" <*>
    m .: "server" <*>
    m .: "actions" <*>
    m .: "logger"
  parseJSON x = fail ("not an object: " ++ show x)

data AnvizConfig = AnvizConfig {
  anviz_ip     :: String,
  anviz_port   :: String,
  anviz_users  :: [[Word8]],
  anviz_enable :: Bool
} deriving Show

instance FromJSON AnvizConfig where
  parseJSON (Object m) = AnvizConfig <$>
    m .: "anviz_ip" <*>
    m .: "anviz_port" <*>
    m .: "anviz_users" <*>
    m .: "anviz_enable"
  parseJSON x = fail ("not an object: " ++ show x)

data SerialConfig = SerialConfig {
  serial_port   :: String,
  serial_users  :: [String],
  serial_enable :: Bool
} deriving Show

instance FromJSON SerialConfig where
  parseJSON (Object m) = SerialConfig <$>
    m .: "serial_port" <*>
    m .: "serial_users" <*>
    m .: "serial_enable"
  parseJSON x = fail ("not an object: " ++ show x)

data ServerConfig = ServerConfig {
  server_eth    :: String,
  server_port   :: String,
  server_enable :: Bool
} deriving Show

instance FromJSON ServerConfig where
  parseJSON (Object m) = ServerConfig <$>
    m .: "server_eth" <*>
    m .: "server_port" <*>
    m .: "server_enable"
  parseJSON x = fail ("not an object: " ++ show x)

data ActionsConfig = ActionsConfig {
  run :: String
} deriving Show

instance FromJSON ActionsConfig where
  parseJSON (Object m) = ActionsConfig <$>
    m .: "run"
  parseJSON x = fail ("not an object: " ++ show x)

data LoggerConfig = LoggerConfig {
  logger_path   :: String,
  logger_level  :: Priority,
  logger_format :: String
} deriving Show

instance FromJSON LoggerConfig where
  parseJSON (Object m) = LoggerConfig <$>
    m .: "logger_path" <*>
    m .: "logger_level" <*>
    m .: "logger_format"
  parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON Priority where
  parseJSON s = case s of
    "DEBUG"     -> pure DEBUG
    "INFO"      -> pure INFO
    "NOTICE"    -> pure NOTICE
    "WARNING"   -> pure WARNING
    "ERROR"     -> pure ERROR
    "CRITICAL"  -> pure CRITICAL
    "ALERT"     -> pure ALERT
    "EMERGENCY" -> pure EMERGENCY
    _           -> fail ("wrong loglevel: " ++ show s ++ ". Must be one of: DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY")
