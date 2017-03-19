module SThread --server part, reads from clients or CThread, writes opposite
( sThread
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.ByteString hiding (putStrLn, putStr)
import Network.Simple.TCP
--import Control.Monad

import Lib(Message(..))

sThread sp chan = serve (HostAny) sp $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Server(sThread): TCP connection established from " ++ show remoteAddr
        t <- forkIO $ do
            c <- dupChan chan
            sReadChan c connectionSocket
        sRead connectionSocket chan t

sRead connectionSocket chan t = do
    --TODO: start chan listener
    a <- recv connectionSocket 410
    case a of
         Nothing    -> do
             putStrLn $ "Server(sRead): got Nothing packet"
             killThread t
         Just a     -> do
             writeChan chan $ Request a
             sRead connectionSocket chan t

sReadChan chan connectionSocket = do
    m <- readChan chan
    case m of
        Responce a -> do
            --putStr $ "Server(sReadChan): "
            --print a
            send connectionSocket a
        _           -> return ()
    sReadChan chan connectionSocket
