module CThread --client part, connecting somewere
( cThread
) where
    
import Control.Concurrent
import Control.Concurrent.Chan
import Network.Simple.TCP
import Data.ByteString hiding (putStrLn, putStr)
import Control.Exception

import Lib(Message(..))

cThread  h dp chan = do
    cBody  h dp chan `finally` do
        putStrLn $ "Client(cThread): connection done or lost or not established"
        threadDelay 3000000
        cThread h dp chan

cBody h dp chan = do
    connect h dp $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Client(cBody): connection established to " ++ show remoteAddr
        t <- forkIO $ do
            c <- dupChan chan
            cReadChan c connectionSocket
        cRead connectionSocket chan t

cRead connectionSocket chan t = do
    a <- recv connectionSocket 410
    case a of
         Nothing    -> do
             putStrLn $ "Client(cRead): got Nothing packet"
             killThread t
         _          -> do
             writeChan chan $ Responce a
             cRead connectionSocket chan t

cReadChan chan connectionSocket = do
    m <- readChan chan
    case m of
        Request (Just a) -> do
            --putStr $ "Client(cReadChan): "
            --print a
            send connectionSocket a
        _           -> return ()
    cReadChan chan connectionSocket
