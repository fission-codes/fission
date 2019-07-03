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
    => Lazy.ByteString
    -> [Opt]
    -> m Lazy.ByteString
run = runHelper byteStringInput

run' :: MonadRIO          cfg m
     => Has IPFS.BinPath  cfg
     => HasProcessContext cfg
     => HasLogFunc        cfg
     => [Opt]
     -> m Lazy.ByteString
run' = runHelper createPipe

runHelper :: Has IPFS.BinPath  cfg
          => HasProcessContext cfg
          => HasLogFunc        cfg
          => MonadRIO          cfg m
          => StreamSpec 'STInput stdin
          -> [Opt]
          -> m Lazy.ByteString
runHelper inStream = ipfsProc readProcessStdout_ inStream byteStringOutput

runExitCode :: MonadRIO          cfg m
            => Has IPFS.BinPath  cfg
            => HasProcessContext cfg
            => HasLogFunc        cfg
            => StreamSpec 'STInput stdin
            -> [Opt]
            -> m ExitCode
runExitCode inStream = ipfsProc runProcess inStream createPipe

ipfsProc :: MonadRIO          cfg m
            => Has IPFS.BinPath  cfg
            => HasProcessContext cfg
            => HasLogFunc        cfg
            => (ProcessConfig stdin stdout () -> m a)
            -> StreamSpec 'STInput stdin
            -> StreamSpec 'STOutput stdout
            -> [Opt]
            -> m a
ipfsProc processor inStream outStream opts = do
  IPFS.BinPath ipfs <- fromConfig

  proc ipfs opts $ processor
                 . setStdin inStream
                 . setStdout outStream
