module Main where

import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import System.Exit
import Data.Yaml

import Lib(Message(..))
import CThread
import MThread
import SThread

main :: IO ()
main = do
    checkArgs
    [h,dp,sp,prog] <- getArgs --host, destport, srcport, prog_to_run
    chan <- newChan
    _ <- forkIO $ cThread h dp chan
    _ <- forkIO $ sThread sp chan
    mThread chan prog
    print "Main: something goes wrong. Bye-bye"

checkArgs = do
    progName <- getProgName
    args <- getArgs
    if "help" `elem` args
        then do
            printHelp progName
            exitSuccess
        else return ()
    if length args /= 4
       then do
            printHelp progName
            die "Wrong number of arguments"
       else return ()

printHelp pn = putStrLn $ "Usage:    "++pn++" dst_host dst_port src_port prog_to_run"
