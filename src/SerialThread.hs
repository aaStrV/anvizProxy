{-# LANGUAGE OverloadedStrings #-}
module SerialThread --serial part, reads from COM-port, sending messages to channel
( serialThread
) where

import System.IO
import System.Hardware.Serialport --hiding (recv)
--import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent hiding(yield)
import Control.Exception
import System.IO.Error
import Control.Monad (forever)

import Lib(Message(..))

serialThread :: String -> Chan Message -> IO ()
serialThread p c = do
  if p=="" 
    then do
      putStrLn $ "Serial(serialThread): empty port, serial thread done"
    else do
      serialConNow p c

serialConNow p c = do
  serialConnect p c `catch` (\e -> do
    if isEOFError e
      then do
        putStrLn $ "Serial(serialConnect): got EOFError, reconnecting now"
        serialConNow p c
      else do
        putStrLn $ "Serial(serialConnect): got some exception, will reconnect after timeout"
        threadDelay 5000000
        putStrLn $ "Serial(serialConnect): reconnecting"
        serialConNow p c )
               
serialConnect p c = do
  h <- hOpenSerial p defaultSerialSettings  { commSpeed = CS9600
                                            , timeout   = 100}
  putStrLn $ "Serial(serialConnect): port "++p++" opened"
  serialRead h c

serialRead :: Handle -> Chan Message -> IO ()
serialRead h c = forever $ do
  l <- hGetLine h
  case l of
    ""   -> return ()
    a    -> writeChan c $ Serial a
