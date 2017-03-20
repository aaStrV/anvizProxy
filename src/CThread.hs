module CThread --client part, connecting somewere
(cThread,
) where
    
import Control.Concurrent
import Control.Concurrent.Chan
import Network.Simple.TCP
import Data.ByteString hiding (putStrLn, putStr)
import Control.Exception
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted

import Lib(Message(..))

reqRecInf = Data.ByteString.pack [0xa5,0x00,0xf6,0x46,0xeb,0x3c,0x00,0x00,0x8e,0xe3]
cBaseTimeout = 10

cThread :: [Char] -> [Char] -> Chan Message -> IO ()
cThread  h dp chan = do
    cBody  h dp chan `finally` do
        --putStrLn $ "Client(cThread): connection done or lost or not established"
        threadDelay 3000000
        cThread h dp chan

cBody :: [Char] -> [Char] -> Chan Message -> IO ()
cBody h dp chan = do
    connect h dp $ \(connectionSocket, remoteAddr) -> do
        --putStrLn $ "Client(cBody): connection established to " ++ show remoteAddr
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
             --putStrLn $ "Client(cRead): got Nothing packet"
             killThread t
         Just a     -> do
             writeChan chan $ Responce a
             oneShotRestart tmsend
             oneShotRestart tmreceave
             cRead connectionSocket chan t tmsend tmreceave

cReadChan chan connectionSocket = do
    m <- readChan chan
    case m of
        Request a -> do
            --putStr $ "Client(cReadChan): "
            --putStrLn a
            send connectionSocket a
        _           -> return ()
    cReadChan chan connectionSocket

cStartWaitResp :: ThreadId -> Socket -> IO()
cStartWaitResp t connectionSocket = do
    --putStrLn "Client(cBody): last request was too long ago. Sending 'Get record information'"
    send connectionSocket reqRecInf
    --Data: a5000000023000002729 - Get the information of T&A device 1
    --Resp: a5:00:f6:46:eb:b0:00:00:12:30:35:2e:31:38:2e:41:31:6f:43:2f:0a:00:02:10:00:00:02:1d:aa

cDisconnect :: ThreadId -> Socket -> IO()
cDisconnect t c = do
    --putStrLn "Client(cDisconnect): closing socket"
    killThread t
    closeSock c
