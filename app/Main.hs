module Main where

import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan

import Lib(Message(..))
import CThread
import MThread

main :: IO ()
main = do
    [h,dp] <- getArgs --host, destport, srcport, prog_to_run
    chan <- newChan
    _ <- forkIO $ cThread h dp chan
    --_ <- forkIO $ sThread sp chan
    mThread chan
    print "Main: Bye-bye"
