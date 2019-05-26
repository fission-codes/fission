{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Random where

import           RIO
import qualified RIO.ByteString as BS

import Data.Word8     (isPrint)
import System.Entropy

text :: Int -> IO ByteString
text amount = getHardwareEntropy amount >>=
  maybe (getEntropy amount) (return . BS.filter isPrint)
