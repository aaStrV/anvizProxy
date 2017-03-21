{-# LANGUAGE OverloadedStrings #-}
module SerialThread --serial part, reads from COM-port, sending messages to channel
( serialThread
) where

import System.IO
import System.Hardware.Serialport --hiding (recv)
--import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent hiding(yield)
import Control.Exception
import System.IO.Error
import Control.Monad (forever)
{-
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Pipes
import qualified Pipes.Prelude as P
-}

import Lib(Message(..))

serialThread :: String -> Chan Message -> IO ()
serialThread p c = do
  if p=="" 
    then do
      putStrLn $ "Serial(serialThread): serial thread exits"
    else do
      serialThread' p c

serialThread' p c = do
  serialBody p c `catch` (\e -> do
    if isEOFError e
       then putStrLn $ "Serial(serialBody): got EOFError"
       else return ()
    if isDoesNotExistError e
       then putStrLn $ "Serial(serialBody): got DoesNotExistError"
       else return ()
    putStrLn ("Caught " ++ show (e :: IOException))
    threadDelay 200000
    serialThread' p c)
               
serialBody p c = do
  --{-
  h <- hOpenSerial p defaultSerialSettings  { commSpeed = CS9600
                                            , timeout   = 100}
  ---}
  --s <- openSerial p defaultSerialSettings  { commSpeed = CS9600 }
  putStrLn $ "Serial(serialBody): port "++p++" opened"
  forever $ do
    l <- hGetLine h
    --l <- recvLn s
    putStr "Serial(serialBody): "
    print l
    --{-
    case l of
      ""   -> return ()
      a    -> writeChan c $ Serial a
    ---}
    {-
    let ls = B.unpack l
    case ls of
      ""   -> return ()
      a    -> writeChan c $ Serial a
    -}
{-
ioGenerator :: IO a -> Producer a IO ()
ioGenerator gen = forever $ lift gen >>= yield

recvLn :: SerialPort -> IO ByteString
recvLn s = P.fold B.append "" id stream
  where stream = ioGenerator (recv s 1) >-> P.takeWhile (/= "\n")
-}
