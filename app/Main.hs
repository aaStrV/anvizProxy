module Main where

import              System.Environment
import              Control.Concurrent
import              Control.Concurrent.Chan
import              System.Exit
import              Data.Yaml
import              Control.Applicative
import              System.Log.Logger
import              System.Log.Handler.Simple
import              System.Log.Handler          (setFormatter)
import              System.Log.Formatter

import              Lib                         (Message(..)
                                                ,Config(..)
                                                ,AnvizConfig(..)
                                                ,SerialConfig(..)
                                                ,ServerConfig(..)
                                                ,ActionsConfig(..)
                                                ,LoggerConfig(..)
                                                ,lcom)
import              CThread
import              MThread
import              SThread
import              SerialThread

setCommonFormatter x form =
  let f = simpleLogFormatter form in
  setFormatter x f

main :: IO ()
main = do
  checkArgs
  [cpath] <- getArgs --path to config file
  c <- readMyConfig cpath
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
    lp = logger_path $ logger c
    ll = logger_level $ logger c
    lf = logger_format $ logger c

  fh <- fileHandler lp DEBUG
  let fh' = setCommonFormatter fh lf
  removeAllHandlers
  updateGlobalLogger lcom $ addHandler fh'
  updateGlobalLogger lcom (setLevel ll)
  
  warningM lcom $ "---------------------------------------------------------------------------"
  noticeM lcom $ "Host: "++h
  noticeM lcom $ "DPort: "++dp
  noticeM lcom $ "SPort: "++sp
  noticeM lcom $ "Run script: "++prun
  noticeM lcom $ "Serial port: "++com
  noticeM lcom $ "Anviz user id's: " ++ (show uss)
  noticeM lcom $ "Serial user id's: " ++ (show suss)
  
  chan <- newChan
  if startAnviz
    then do
      _ <- forkIO $ cThread h dp chan
      noticeM lcom $ "Main: anviz client started"
      return ()
    else do
      return ()
  if startServer
    then do
      _ <- forkIO $ sThread sp chan
      noticeM lcom $ "Main: server started"
      return ()
    else do
      return ()
  if startSerial
    then do
      _ <- forkIO $ serialThread com chan
      noticeM lcom $ "Main: serial started"
      return ()
    else do
      return ()
  if not (startAnviz || startServer || startSerial)
    then do
      noticeM lcom "Main: Nothing to do, check config"
    else do
      mThread chan uss suss prun
  noticeM lcom "Main: something goes wrong. Bye-bye"

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

printHelp pn = do
  putStrLn $ "Usage:    "++pn++" <path to config.yaml>"

readMyConfig :: String -> IO Config
readMyConfig path =
  either (error . show) id <$>
  decodeFileEither path
