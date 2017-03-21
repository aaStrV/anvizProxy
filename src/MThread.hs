module MThread --middle part, analizing messages and traffic
( mThread
) where

import            Control.Concurrent.Chan
import qualified  Data.ByteString         as B
import            Control.Monad
import            System.Process
import            GHC.Word

import            Lib                     (Message(..))

mThread chan uss suss p = do
  forever $ do
    msg <- readChan chan
    case msg of
      Request message     -> do
        {-putStr "Middle(mBoby): Request "
        let m = B.unpack message
        print m-}
        return ()
      Responce message    -> do
        let m = B.unpack message
        {-putStr "Middle(mBoby): Responce "
        print m-}
        if analizeResp m uss
          then do
            --putStrLn "Middle(mBoby): Alarm!"
            runExt p
          else return ()
      Serial message      -> do
        putStr "Middle(mBoby): Serial "
        print message
        if analizeSerial suss message
          then do
            --putStrLn "Middle(mBoby): Alarm!"
            runExt p
          else return ()

analizeResp :: [Word8] -> [[Word8]] -> Bool
analizeResp [] _ = False
analizeResp _ [] = False
analizeResp (0xa5:_:_:_:_:0xdf:0x00:0x00:0x0e:xs) uss = take 5 xs `elem` uss
analizeResp _ _ = False

analizeSerial :: [String] -> String -> Bool
analizeSerial _ "" = False
analizeSerial [] _ = False
analizeSerial xs a = a `elem` xs

runExt :: String -> IO ()
runExt p = do
  _ <- createProcess (shell p)
  return ()
