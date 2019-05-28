{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Random (byteString, byteString') where

import           RIO
import qualified RIO.ByteString as BS

import qualified Data.ByteString.Random as BS
import           Data.Word8             (isPrint)
import           System.Entropy

byteString' :: Natural -> IO ByteString
byteString' amount = return . BS.filter isPrint =<< BS.random amount

byteString :: Int -> IO ByteString
byteString amount = getHardwareEntropy amount >>=
  maybe (getEntropy amount) (return . BS.filter isPrint)
