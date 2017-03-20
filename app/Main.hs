module Main where

import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import System.Exit
import Data.Yaml
import Control.Applicative

import Lib  (Message(..)
            ,Config(..)
            ,AnvizConfig(..)
            ,SerialConfig(..)
            ,ServerConfig(..)
            ,ActionsConfig(..))
import CThread
import MThread
import SThread

main :: IO ()
main = do
    checkArgs
    [cpath] <- getArgs --path to config file
    c <- readMyConfig cpath
    --print c
    let
        h = anviz_ip $ anviz c
        dp = anviz_port $ anviz c
        sp = server_port $ server c
        prun = run $ actions c
    putStrLn $ "Host: "++h
    putStrLn $ "DPort: "++dp
    putStrLn $ "SPort: "++sp
    putStrLn $ "Run script: "++prun
    chan <- newChan
    _ <- forkIO $ cThread h dp chan
    _ <- forkIO $ sThread sp chan
    mThread chan prun
    print "Main: something goes wrong. Bye-bye"

checkArgs = do
    progName <- getProgName
    args <- getArgs
    if "--help" `elem` args
        then do
            printHelp progName
            exitSuccess
        else return ()
    if length args /= 1
       then do
            printHelp progName
            die "Wrong number of arguments"
       else return ()

printHelp pn = putStrLn $ "Usage:    "++pn++" path_to_config_yaml"

readMyConfig :: String -> IO Config
readMyConfig path =
    either (error . show) id <$>
    decodeFileEither path--"./src/config.yaml"
