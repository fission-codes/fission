module Fission.IPFS.Process (run, run') where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has
import System.Process.Typed

import Fission
import Fission.Internal.Constraint
import Fission.IPFS.Types as IPFS

run :: MonadRIO cfg m
    => Has IPFS.Path cfg
    => [Opt]
    -> Lazy.ByteString
    -> m Lazy.ByteString
run opts input = runHelper opts $ byteStringInput input

run' :: MonadRIO cfg m
     => Has IPFS.Path cfg
     => [Opt]
     -> m Lazy.ByteString
run' opts = runHelper opts createPipe

runHelper :: Has IPFS.Path cfg
          => MonadRIO cfg m
          => [Opt]
          -> StreamSpec 'STInput stdin
          -> m Lazy.ByteString
runHelper opts inStream = do
  IPFS.Path ipfs <- fromConfig

  readProcessStdout_
    . setStdin inStream
    . setStdout byteStringOutput
    $ proc ipfs opts
