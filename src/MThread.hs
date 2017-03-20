module MThread --middle part, analizing messages and traffic
( mThread
) where

import Control.Concurrent.Chan
import Data.ByteString as B hiding (putStrLn, putStr, take)
import Control.Monad
import System.Process
import GHC.Word

import Lib(Message(..))

mThread chan uss p = do
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
            Serial message      -> return ()

analizeResp :: [Word8] -> [[Word8]] -> Bool
analizeResp [] _ = False
analizeResp _ [] = False
analizeResp (0xa5:idhh:idhl:idlh:idll:0xdf:0x00:0x00:0x0e:xs) (us:uss)
    | take 5 xs == us   = True
    | otherwise         = analizeResp (0xa5:idhh:idhl:idlh:idll:0xdf:0x00:0x00:0x0e:xs) uss
analizeResp _ _ = False

runExt :: String -> IO ()
runExt p = do
    _ <- createProcess (shell p)
    return ()
