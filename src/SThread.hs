module SThread --server part, reads from clients or CThread, writes opposite
( sThread
) where

import            Control.Concurrent
import            Control.Concurrent.Chan
import qualified  Data.ByteString         as B
import            Network.Simple.TCP
import            System.Log.Logger

import            Lib                     (Message(..), lcom)

sThread sp chan = serve (HostAny) sp $ \(connectionSocket, remoteAddr) -> do
  noticeM lcom $ "Server(sThread): TCP connection established from " ++ (show remoteAddr)
  t <- forkIO $ do
    c <- dupChan chan
    sReadChan c connectionSocket
  sRead connectionSocket chan t

sRead connectionSocket chan t = do
  a <- recv connectionSocket 410
  case a of
    Nothing    -> do
      warningM lcom $ "Server(sRead): got Nothing packet"
      killThread t
    Just a     -> do
      writeChan chan $ Request a
      sRead connectionSocket chan t

sReadChan chan connectionSocket = do
  m <- readChan chan
  case m of
    Responce a  -> do
      noticeM lcom $ "Server(sReadChan): " ++ (show a)
      send connectionSocket a
    _           -> return ()
  sReadChan chan connectionSocket
