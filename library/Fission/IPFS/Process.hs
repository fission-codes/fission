module Fission.IPFS.Process
  ( run
  , run'
  , run_
  , runExitCode
  , runErr
  , runErr'
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process

import Flow
import Data.Has

import qualified Fission.Config as Config
import           Fission.Internal.Process
import           Fission.IPFS.Types as IPFS

run :: RIOProc cfg m
    => Has IPFS.BinPath cfg
    => Has IPFS.Timeout cfg
    => [Opt]
    -> Lazy.ByteString
    -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run opts arg = runBS (byteStringInput arg) opts

run' :: RIOProc cfg m
     => Has IPFS.BinPath cfg
     => Has IPFS.Timeout cfg
     => [Opt]
     -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run' = runBS createPipe

run_ :: RIOProc          cfg m
     => Has IPFS.BinPath cfg
     => Has IPFS.Timeout cfg
     => [Opt]
     -> Lazy.ByteString
     -> m ExitCode
run_ opts arg = runExitCode (byteStringInput arg) opts

runBS :: RIOProc cfg m
      => Has IPFS.BinPath cfg
      => Has IPFS.Timeout cfg
      => StreamIn stdin
      -> [Opt]
      -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
runBS inStream = ipfsProc readProcess inStream byteStringOutput

runExitCode :: RIOProc          cfg m
            => Has IPFS.BinPath cfg
            => Has IPFS.Timeout cfg
            => StreamIn stdin
            -> [Opt]
            -> m ExitCode
runExitCode inStream = ipfsProc runProcess inStream createPipe

runErr' :: RIOProc        cfg m
      => Has BinPath      cfg
      => Has IPFS.Timeout cfg
      => [Opt]
      -> Lazy.ByteString
      -> m (ExitCode, Lazy.ByteString)
runErr' opts arg = runErr (byteStringInput arg) opts

runErr :: RIOProc cfg m
       => Has BinPath cfg
       => Has IPFS.Timeout cfg
       => StreamIn stdin
       -> [Opt]
       -> m (ExitCode, Lazy.ByteString)
runErr inStream = ipfsProc readProcessStderr inStream byteStringOutput

ipfsProc :: RIOProc          cfg m
         => Has IPFS.BinPath cfg
         => Has IPFS.Timeout cfg
         => (ProcessConfig stdin stdout () -> m a)
         -> StreamIn  stdin
         -> StreamOut stdout
         -> [Opt]
         -> m a
ipfsProc processor inStream outStream opts = do
  IPFS.BinPath ipfs <- Config.get
  IPFS.Timeout secs <- Config.get
  let opts' = ("--timeout=" <> show secs <> "s") : opts
  proc ipfs opts' <| processor
                  <. setStdin  inStream
                  <. setStdout outStream
