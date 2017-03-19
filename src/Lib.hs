module Lib
    ( Message(..)
    ) where

import Data.ByteString hiding (putStrLn)

data Message = Request ByteString
             | Responce ByteString
