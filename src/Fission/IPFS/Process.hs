{-# LANGUAGE NoImplicitPrelude #-}

module Fission.IPFS.Process where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import System.Process.Typed

type Opt = String

run :: MonadIO m => [Opt] -> Lazy.ByteString -> m Lazy.ByteString
run opts bs = readProcessStdout_
            . setStdin (byteStringInput bs)
            . setStdout byteStringOutput
            $ proc "/usr/local/bin/ipfs" opts

run' :: MonadIO m => [Opt] -> m Lazy.ByteString
run' opts = readProcessStdout_
          . setStdin createPipe
          . setStdout byteStringOutput
          $ proc "/usr/local/bin/ipfs" opts
