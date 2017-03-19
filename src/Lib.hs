module Lib
    ( Message(..)
    ) where

import Data.ByteString hiding (putStrLn)

data Message = Request (Maybe ByteString)
             | Responce (Maybe ByteString)
