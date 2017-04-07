{-# LANGUAGE OverloadedStrings #-}
module SerialThread --serial part, reads from COM-port, sending messages to channel
( serialThread
) where

import           Control.Concurrent         hiding (yield)
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad              (forever)
import           System.Hardware.Serialport
import           System.IO
import           System.IO.Error
import           System.Log.Logger

import           Lib                        (Message (..), lcom)

serialThread :: String -> Chan Message -> IO ()
serialThread p c =
    if p==""
        then warningM lcom "Serial(serialThread): empty port, serial thread done"
        else serialConNow p c

serialConNow p c =
    serialConnect p c `catch` (\e ->
        if isEOFError e
        then do
            noticeM lcom "Serial(serialConnect): got EOFError, reconnecting now"
            serialConNow p c
        else do
            noticeM lcom "Serial(serialConnect): got some exception, will reconnect after timeout"
            threadDelay 5000000
            noticeM lcom "Serial(serialConnect): reconnecting"
            serialConNow p c )

serialConnect p c = do
    h <- hOpenSerial p defaultSerialSettings  { commSpeed = CS9600
                                                , timeout   = 200}
    noticeM lcom $ "Serial(serialConnect): port "++p++" opened"
    serialRead h c

serialRead :: Handle -> Chan Message -> IO ()
serialRead h c = forever $ do
    l <- hGetLine h
    case filter (/='\r') l of
        ""   -> do
            infoM lcom "Serial(serialRead): got 'alive' signal"
            return ()
        a    -> writeChan c $ Serial a
