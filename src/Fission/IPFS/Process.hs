module Fission.IPFS.Process
  ( run
  , run'
  , run_
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process

import Data.Has

import qualified Fission.Config as Config
import           Fission.Internal.Process
import           Fission.IPFS.Types as IPFS

run :: (RIOProc cfg m, Has IPFS.BinPath cfg) => [Opt] -> Lazy.ByteString -> m Lazy.ByteString
run opts arg = runBS (byteStringInput arg) opts

run' :: (RIOProc cfg m, Has IPFS.BinPath cfg) => [Opt] -> m Lazy.ByteString
run' = runBS createPipe

run_ :: (RIOProc cfg m, Has IPFS.BinPath cfg) => [Opt] -> Lazy.ByteString -> m ExitCode
run_ opts arg = runExitCode (byteStringInput arg) opts

runBS :: (RIOProc cfg m, Has IPFS.BinPath cfg) => StreamIn stdin -> [Opt] -> m Lazy.ByteString
runBS inStream = ipfsProc readProcessStdout_ inStream byteStringOutput

runExitCode :: (RIOProc cfg m, Has IPFS.BinPath cfg) => StreamIn stdin -> [Opt] -> m ExitCode
runExitCode inStream = ipfsProc runProcess inStream createPipe

ipfsProc :: RIOProc cfg m
         => Has IPFS.BinPath cfg
         => (ProcessConfig stdin stdout () -> m a)
         -> StreamIn  stdin
         -> StreamOut stdout
         -> [Opt]
         -> m a
ipfsProc processor inStream outStream opts = do
  IPFS.BinPath ipfs <- Config.get
  proc ipfs opts $ processor
                 . setStdin  inStream
                 . setStdout outStream
