module Main where

import System.Environment
import Network.Simple.TCP
--import Network.Socket hiding (connect,recv,send,listen,accept)
import Data.ByteString hiding (putStrLn, putStr)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception

import Lib(Message(..))
import CThread

--data Message = Request (Maybe ByteString)
--             | Responce (Maybe ByteString)

main :: IO ()
main = do
    [h,dp] <- getArgs --host, destport, srcport, prog_to_run
    chan <- newChan
    _ <- forkIO $ cThread h dp chan
    --_ <- forkIO $ sThread sp chan
    mThread chan
    print "Main: Bye-bye"

mThread chan = do
    mBody chan --`finally` do
        --putStrLn $ "Restarting middle server"
        --mThread chan

mBody chan = do
    --m <- dupChan chan
    forever $ do
        msg <- readChan chan
        case msg of
             Request (Just message)     -> print "ok"
             Responce (Just message)    -> do
                 putStr "Middle(mBoby): "
                 print message
             Responce Nothing           -> print "Got Nothing message :/"
             _                          -> print "Unknown message receaved"
