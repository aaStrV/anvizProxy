module SThread --server part, reads from clients or CThread, writes opposite
( sThread
) where

import Control.Concurrent.Chan
import Data.ByteString hiding (putStrLn, putStr)
import Control.Monad

import Lib(Message(..))

sThread sp chan = do
    sBody chan --`finally` do
        --putStrLn $ "Restarting middle server"
        --mThread chan

sBody chan = return ()
