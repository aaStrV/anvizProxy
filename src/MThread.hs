{-|
Module      : MThread
Description : Middle thread
Copyright   : (c) telegatrollej@yandex.ru, 2017
License     : BSD
Maintainer  : telegatrollej@yandex
Stability   : experimental
Portability : POSIX

Middle part, analizing messages and traffic.
-}
module MThread
( mThread
) where

import           Control.Concurrent.Chan
import           Control.Monad
import qualified Data.ByteString         as B
import           GHC.Word
import           System.Log.Logger
import           System.Process

import           Lib                     (Message (..), lcom)

mThread :: Chan Message -> [[Word8]] -> [String] -> String -> IO ()
mThread chan uss suss p = forever $ do
    msg <- readChan chan
    case msg of
        Request message     ->
            infoM lcom $ "Middle(mBoby): Request " ++ show (B.unpack message)
        Responce message    -> do
            let m = B.unpack message
            infoM lcom $ "Middle(mBoby): Responce " ++ show m
            when (analizeResp m uss) $ do
                alertM lcom $ "Middle(mBoby): Alarm! " ++ show m
                runExt p
        Serial message      -> do
            infoM lcom $ "Middle(mBoby): Serial " ++ show message
            when (analizeSerial suss message) $ do
                alertM lcom $ "Middle(mBoby): Alarm!" ++ show message
                runExt p

runExt :: String -> IO ()
runExt p = do
    _ <- createProcess (shell p)
    return ()

--------------------------------------------------------------------------------
--Pure part
--------------------------------------------------------------------------------
analizeResp :: [Word8] -> [[Word8]] -> Bool
analizeResp [] _                                      = False
analizeResp _ []                                      = False
analizeResp (0xa5:_:_:_:_:0xdf:0x00:0x00:0x0e:xs) uss = take 5 xs `elem` uss
analizeResp _ _                                       = False

analizeSerial :: [String] -> String -> Bool
analizeSerial _ "" = False
analizeSerial [] _ = False
analizeSerial xs a = a `elem` xs
