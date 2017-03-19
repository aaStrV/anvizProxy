module MThread --middle part, analizing messages and traffic
( mThread
) where

import Control.Concurrent.Chan
import Data.ByteString hiding (putStrLn, putStr)
import Control.Monad

import Lib(Message(..))

mThread chan p = do
    mBody chan p --`finally` do
        --putStrLn $ "Restarting middle server"
        --mThread chan

mBody chan p = do
    forever $ do
        msg <- readChan chan
        case msg of
             Request (Just message)     -> do
                 putStr "Middle(mBoby): Request "
                 print message
             Responce (Just message)    -> do
                 putStr "Middle(mBoby): Responce "
                 print message
             Responce Nothing           -> print "Got Nothing message :/"
             _                          -> print "Unknown message receaved"

analizeReq m = m
