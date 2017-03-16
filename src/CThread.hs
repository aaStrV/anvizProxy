module CThread --client part, connecting somewere
( cThread
) where
    
import Control.Concurrent
import Control.Concurrent.Chan
import Network.Simple.TCP
import Control.Exception
import Data.ByteString hiding (putStrLn, putStr)

import Lib(Message(..))

cThread  h dp chan = do
    cBody  h dp chan `finally` do
        putStrLn $ "Client(cThread): connection done or lost or not established"
        threadDelay 3000000
        cThread h dp chan

cBody h dp chan = do
    connect h dp $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Client(cBody): connection established to " ++ show remoteAddr
        cRead connectionSocket chan

cRead connectionSocket chan = do
    a <- recv connectionSocket 410
    case a of
         Nothing    -> putStrLn $ "Client(cRead): got Nothing packet"
         _          -> do
             writeChan chan $ Responce a
             cRead connectionSocket chan
