module Main where

import              System.Environment
import              Control.Concurrent
import              Control.Concurrent.Chan
import              System.Exit
import              Data.Yaml
import              Control.Applicative
--import              System.Log.Logger

import              Lib                         (Message(..)
                                                ,Config(..)
                                                ,AnvizConfig(..)
                                                ,SerialConfig(..)
                                                ,ServerConfig(..)
                                                ,ActionsConfig(..))
import              CThread
import              MThread
import              SThread
import              SerialThread

main :: IO ()
main = do
  checkArgs
  [cpath] <- getArgs --path to config file
  c <- readMyConfig cpath
  --print c
  let
    h = anviz_ip $ anviz c
    dp = anviz_port $ anviz c
    uss = anviz_users $ anviz c
    sp = server_port $ server c
    com = serial_port $ serial c
    suss = serial_users $ serial c
    prun = run $ actions c
    startAnviz = anviz_enable $ anviz c
    startServer = server_enable $ server c
    startSerial = serial_enable $ serial c
  {-
  putStrLn $ "Host: "++h
  putStrLn $ "DPort: "++dp
  putStrLn $ "SPort: "++sp
  putStrLn $ "Run script: "++prun
  putStrLn $ "Serial port: "++com
  putStr "Anviz user id's: "
  print uss
  putStr "Serial user id's: "
  print suss
  -}
  chan <- newChan
  if startAnviz
    then do
      _ <- forkIO $ cThread h dp chan
      putStrLn $ "Main: anviz client started"
      return ()
    else do
      return ()
  if startServer
    then do
      _ <- forkIO $ sThread sp chan
      putStrLn $ "Main: server started"
      return ()
    else do
      return ()
  if startSerial
    then do
      _ <- forkIO $ serialThread com chan
      putStrLn $ "Main: serial started"
      return ()
    else do
      return ()
  if not (startAnviz || startServer || startSerial)
    then do
      putStrLn "Main: Nothing to do, check config"
    else do
      mThread chan uss suss prun
  putStrLn "Main: something goes wrong. Bye-bye"

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
