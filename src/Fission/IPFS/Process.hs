module Fission.IPFS.Process (run, run') where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has
import System.Process.Typed

import Fission.Internal.Constraint
import Fission.Config

type Opt = String

run :: MonadRIO cfg m
    => Has IpfsPath cfg
    => [Opt]
    -> Lazy.ByteString
    -> m Lazy.ByteString
run opts input = runHelper opts $ byteStringInput input

run' :: MonadRIO cfg m
     => Has IpfsPath cfg
     => [Opt]
     -> m Lazy.ByteString
run' opts = runHelper opts createPipe

runHelper :: Has IpfsPath cfg
          => MonadRIO cfg m
          => [Opt]
          -> StreamSpec 'STInput stdin
          -> m Lazy.ByteString
runHelper opts inStream = do
  IpfsPath ipfs <- fromCfg

  readProcessStdout_
    . setStdin inStream
    . setStdout byteStringOutput
    $ proc ipfs opts
