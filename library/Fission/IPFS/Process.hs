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

import SuperRecord

import qualified Fission.Config as Config
import           Fission.Internal.Process
import           Fission.IPFS.Types as IPFS

run :: RIOProc      (Rec cfg) m
    => Has "ipfsPath"    cfg IPFS.BinPath
    => Has "ipfsTimeout" cfg IPFS.Timeout
    => [Opt]
    -> Lazy.ByteString
    -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run opts arg = runBS (byteStringInput arg) opts

run' :: RIOProc (Rec cfg) m
     => Has "ipfsPath"    cfg IPFS.BinPath
     => Has "ipfsTimeout" cfg IPFS.Timeout
     => [Opt]
     -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run' = runBS createPipe

run_ :: RIOProc          (Rec cfg) m
     => Has "ipfsPath"    cfg IPFS.BinPath
     => Has "ipfsTimeout" cfg IPFS.Timeout
     => [Opt]
     -> Lazy.ByteString
     -> m ExitCode
run_ opts arg = runExitCode (byteStringInput arg) opts

runBS :: RIOProc (Rec cfg) m
      => Has "ipfsPath"    cfg IPFS.BinPath
      => Has "ipfsTimeout" cfg IPFS.Timeout
      => StreamIn stdin
      -> [Opt]
      -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
runBS inStream = ipfsProc readProcess inStream byteStringOutput

runExitCode :: RIOProc      (Rec cfg) m
            => Has "ipfsPath"    cfg IPFS.BinPath
            => Has "ipfsTimeout" cfg IPFS.Timeout
            => StreamIn stdin
            -> [Opt]
            -> m ExitCode
runExitCode inStream = ipfsProc runProcess inStream createPipe

runErr' :: RIOProc      (Rec cfg) m
        => Has "ipfsPath"    cfg IPFS.BinPath
        => Has "ipfsTimeout" cfg IPFS.Timeout
        => [Opt]
        -> Lazy.ByteString
        -> m (ExitCode, Lazy.ByteString)
runErr' opts arg = runErr (byteStringInput arg) opts

runErr :: RIOProc      (Rec cfg) m
       => Has "ipfsPath"    cfg IPFS.BinPath
       => Has "ipfsTimeout" cfg IPFS.Timeout
       => StreamIn stdin
       -> [Opt]
       -> m (ExitCode, Lazy.ByteString)
runErr inStream = ipfsProc readProcessStderr inStream byteStringOutput

ipfsProc :: RIOProc      (Rec cfg) m
         => Has "ipfsPath"    cfg IPFS.BinPath
         => Has "ipfsTimeout" cfg IPFS.Timeout
         => (ProcessConfig stdin stdout () -> m a)
         -> StreamIn  stdin
         -> StreamOut stdout
         -> [Opt]
         -> m a
ipfsProc processor inStream outStream opts = do
  IPFS.BinPath ipfs <- asksR #ipfsPath
  IPFS.Timeout secs <- asksR #ipfsTimeout
  let opts' = ("--timeout=" <> show secs <> "s") : opts
  proc ipfs opts' $ processor
                  . setStdin  inStream
                  . setStdout outStream
