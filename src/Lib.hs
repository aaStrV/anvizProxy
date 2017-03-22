{-# LANGUAGE OverloadedStrings #-}
module              Lib                     ( Message(..)
                                            , Config(..)
                                            , AnvizConfig(..)
                                            , SerialConfig(..)
                                            , ServerConfig(..)
                                            , ActionsConfig(..)
                                            ) where

import              Data.ByteString         as B
import              Data.Yaml
import              Control.Applicative
import              GHC.Word

data Message = Request ByteString
             | Responce ByteString
             | Serial String

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
  anviz_ip ::     String,
  anviz_port ::   String,
  anviz_users ::  [[Word8]],
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
  serial_port :: String,
  serial_users :: [String],
  serial_enable :: Bool
} deriving Show

instance FromJSON SerialConfig where
  parseJSON (Object m) = SerialConfig <$>
    m .: "serial_port" <*>
    m .: "serial_users" <*>
    m .: "serial_enable"
  parseJSON x = fail ("not an object: " ++ show x) 

data ServerConfig = ServerConfig {
  server_eth :: String,
  server_port :: String,
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
