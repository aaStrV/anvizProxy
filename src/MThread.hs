module MThread
( mThread
) where
    
--import Control.Concurrent
import Control.Concurrent.Chan
--import Network.Simple.TCP
--import Control.Exception
import Data.ByteString hiding (putStrLn, putStr)
import Control.Monad

import Lib(Message(..))

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
