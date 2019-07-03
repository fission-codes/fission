module Fission.IPFS.Process
  ( run
  , run'
  , runExitCode
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process

import Data.Has

import Fission
import Fission.Internal.Constraint
import Fission.IPFS.Types as IPFS

run :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.BinPath  cfg
    => [Opt]
    -> Lazy.ByteString
    -> m Lazy.ByteString
run opts input = runHelper opts $ byteStringInput input

run' :: MonadRIO          cfg m
     => Has IPFS.BinPath  cfg
     => HasProcessContext cfg
     => HasLogFunc        cfg
     => [Opt]
     -> m Lazy.ByteString
run' opts = runHelper opts createPipe

runHelper :: Has IPFS.BinPath  cfg
          => HasProcessContext cfg
          => HasLogFunc        cfg
          => MonadRIO          cfg m
          => [Opt]
          -> StreamSpec 'STInput stdin
          -> m Lazy.ByteString
runHelper opts inStream = do
  IPFS.BinPath ipfs <- fromConfig

  proc ipfs opts $ readProcessStdout_
                 . setStdin inStream
                 . setStdout byteStringOutput

runExitCode :: MonadRIO          cfg m
            => Has IPFS.BinPath  cfg
            => HasProcessContext cfg
            => HasLogFunc        cfg
            => [Opt]
            -> Lazy.ByteString
            -> m ExitCode
runExitCode opts input = do
  IPFS.BinPath ipfs <- fromConfig

  proc ipfs opts $ runProcess
                 . setStdin (byteStringInput input)
                 . setStdout createPipe
