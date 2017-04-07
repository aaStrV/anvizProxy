{-|
Module      : CThread
Description : Client thread
Copyright   : (c) telegatrollej@yandex.ru, 2017
License     : BSD
Maintainer  : telegatrollej@yandex
Stability   : experimental
Portability : POSIX

Client part, connecting to anviz biostation.
-}
module CThread
(cThread,
) where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.Suspend.Lifted
import           Control.Concurrent.Timer
import           Control.Exception
import qualified Data.ByteString                   as B
import           Network.Simple.TCP
import           System.Log.Logger

import           Lib                               (Message (..), lcom)

reqRecInf = B.pack [0xa5,0x00,0x00,0x00,0x02,0x30,0x00,0x00,0x27,0x29]
cBaseTimeout = 10

cThread :: [Char] -> [Char] -> Chan Message -> IO ()
cThread  h dp chan = do
  cBody  h dp chan `finally` do
    warningM lcom "Client(cThread): connection done or lost or not established"
    threadDelay 3000000
    cThread h dp chan

cBody :: [Char] -> [Char] -> Chan Message -> IO ()
cBody h dp chan = do
  connect h dp $ \(connectionSocket, remoteAddr) -> do
    warningM lcom $ "Client(cBody): connection established to " ++ show remoteAddr
    t <- forkIO $ do
        c <- dupChan chan
        cReadChan c connectionSocket
    tmsend <- newTimer
    tmreceave <- newTimer
    oneShotStart tmsend (cStartWaitResp t connectionSocket) $ sDelay cBaseTimeout
    oneShotStart tmreceave (cDisconnect t connectionSocket) $ sDelay $ cBaseTimeout+1
    cRead connectionSocket chan t tmsend tmreceave

cRead connectionSocket chan t tmsend tmreceave = do
  m <- recv connectionSocket 410
  case m of
    Nothing    -> do
      debugM lcom $ "Client(cRead): got Nothing packet"
      killThread t
    Just a     -> do
      writeChan chan $ Responce a
      oneShotRestart tmsend
      oneShotRestart tmreceave
      debugM lcom $ "Client(cRead): got " ++ (show $ B.unpack a)
      cRead connectionSocket chan t tmsend tmreceave

cReadChan :: Chan Message -> Socket -> IO ()
cReadChan chan connectionSocket = do
  m <- readChan chan
  case m of
    Request a -> do
      send connectionSocket a
      debugM lcom $ "Client(cReadChan): " ++ (show a)
    _           -> return ()
  cReadChan chan connectionSocket

cStartWaitResp :: ThreadId -> Socket -> IO()
cStartWaitResp t connectionSocket = do
  noticeM lcom "Client(cBody): last request was too long ago. Sending 'Get the information of T&A device 1'"
  send connectionSocket reqRecInf
  --Get record information: 0xa5,0x00,0xf6,0x46,0xeb,0x3c,0x00,0x00,0x8e,0xe3
  --Data: a5000000023000002729 - Get the information of T&A device 1
  --Resp: a5:00:f6:46:eb:b0:00:00:12:30:35:2e:31:38:2e:41:31:6f:43:2f:0a:00:02:10:00:00:02:1d:aa

cDisconnect :: ThreadId -> Socket -> IO()
cDisconnect t c = do
  warningM lcom "Client(cDisconnect): closing socket"
  killThread t
  closeSock c
