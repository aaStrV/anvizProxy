--module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Data.Yaml
import           System.Environment
import           System.Exit
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import           CThread
import           Lib                       (ActionsConfig (..),
                                            AnvizConfig (..), Config (..),
                                            LoggerConfig (..), Message (..),
                                            SerialConfig (..),
                                            ServerConfig (..), lcom)
import           MThread
import           SerialThread
import           SThread

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
  warningM lcom
    "------------------------------------------------------------------------"
  noticeM lcom $ "Host: "++h
  noticeM lcom $ "DPort: "++dp
  noticeM lcom $ "SPort: "++sp
  noticeM lcom $ "Run script: "++prun
  noticeM lcom $ "Serial port: "++com
  noticeM lcom $ "Anviz user id's: " ++ show uss
  noticeM lcom $ "Serial user id's: " ++ show suss
  chan <- newChan
  when startAnviz $ do
    forkIO $ cThread h dp chan
    noticeM lcom "Main: anviz client started"
  when startServer $ do
    forkIO $ sThread sp chan
    noticeM lcom "Main: server started"
  when startSerial $ do
      forkIO $ serialThread com chan
      noticeM lcom "Main: serial started"
  if not (startAnviz || startServer || startSerial)
    then noticeM lcom "Main: Nothing to do, check config"
    else mThread chan uss suss prun
  noticeM lcom "Main: something goes wrong. Bye-bye"

checkArgs :: IO ()
checkArgs = do
  progName <- getProgName
  args <- getArgs
  when ("--help" `elem` args) $ do
    printHelp progName
    exitSuccess
  when (length args /= 1) $ do
    printHelp progName
    putStrLn "Wrong number of arguments"
    exitFailure

printHelp :: String -> IO ()
printHelp pn =
  putStrLn $ "Usage:    "++pn++" <path to config.yaml>"

readMyConfig :: String -> IO Config
readMyConfig path =
  either (error . show) id <$>
  decodeFileEither path
