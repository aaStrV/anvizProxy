module SerialThread --serial part, reads from COM-port, sending messages to channel
( serialThread
) where

import System.IO
import System.Hardware.Serialport
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception

import Lib(Message(..))

serialThread :: String -> Chan Message -> IO ()
serialThread p c = do
    if p=="" 
       then return ()
       else do
           return ()
               
