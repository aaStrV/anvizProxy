module MThread --middle part, analizing messages and traffic
( mThread
) where

import Control.Concurrent.Chan
import Data.ByteString as B hiding (putStrLn, putStr)
import Control.Monad
import System.Process

import Lib(Message(..))

mThread chan p = do
    mBody chan p --`finally` do
        --putStrLn $ "Restarting middle server"
        --mThread chan

mBody chan p = do
    forever $ do
        msg <- readChan chan
        case msg of
             Request message     -> do
                 --putStr "Middle(mBoby): Request "
                 return ()
             Responce message    -> do
                 --putStr "Middle(mBoby): Responce "
                 --print message
                 let m = B.unpack message
                 --print m
                 if analizeResp m
                    then do
                        putStrLn "Middle(mBoby): Alarm!"
                        _ <- createProcess (proc "ls" [])
                        return ()
                    else return ()

--analizeResp :: [Word8] -> Bool
analizeResp [] = False
--analizeResp (x:xs) = if x == 0xa5 then True else False
analizeResp (0xa5:_:_:_:_:0xdf:0x00:0x00:0x0e:0x00:0x00:0x00:0x00:0x08:xs) = True
analizeResp _ = False

--checkResp :: [a] -> [a]
