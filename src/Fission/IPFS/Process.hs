module Fission.IPFS.Process (run, run') where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has
import System.Process.Typed

import Fission.Config
import Fission.Internal.Constraint
import Fission.IPFS.Types

run :: MonadRIO cfg m
    => Has IPFSPath cfg
    => [Opt]
    -> Lazy.ByteString
    -> m Lazy.ByteString
run opts input = runHelper opts $ byteStringInput input

run' :: MonadRIO cfg m
     => Has IPFSPath cfg
     => [Opt]
     -> m Lazy.ByteString
run' opts = runHelper opts createPipe

runHelper :: Has IPFSPath cfg
          => MonadRIO cfg m
          => [Opt]
          -> StreamSpec 'STInput stdin
          -> m Lazy.ByteString
runHelper opts inStream = do
  IPFSPath ipfs <- fromCfg

  readProcessStdout_
    . setStdin inStream
    . setStdout byteStringOutput
    $ proc ipfs opts
