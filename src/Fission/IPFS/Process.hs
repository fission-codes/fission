{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.IPFS.Process (run, run') where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import System.Process.Typed

import Fission.Internal.Constraint
import Fission.Env

type Opt = String

run :: (WithRIO env m, HasIPFSBin env) => [Opt] -> Lazy.ByteString -> m Lazy.ByteString
run opts input = runHelper opts (byteStringInput input)

run' :: (WithRIO env m, HasIPFSBin env) => [Opt] -> m Lazy.ByteString
run' opts = runHelper opts createPipe

runHelper :: WithRIO env m
     => HasIPFSBin env
     => [Opt]
     -> StreamSpec 'STInput stdin
     -> m Lazy.ByteString
runHelper opts stdIn = do
  ipfsPath <- asks $ view ipfsBinL

  readProcessStdout_
    . setStdin stdIn
    . setStdout byteStringOutput
    $ proc ipfsPath opts
