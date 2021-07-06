module Fission.CLI.Environment.IPFS (binCidFor) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Prelude

import           Fission.CLI.Environment.OS as OS

binCidFor :: OS.Supported -> URL -- Or something
binCidFor =
  IPFS.CID . \case
    Linux -> "https://github.com/ipfs/go-ipfs/releases/download/v0.9.0/go-ipfs_v0.9.0_linux-amd64.tar.gz" -- 0.9
    NixOS -> "bafybeicxpcyuibrrml2k4onfkbenqp35c7umguhizbnreptdy4ttxkb674" -- 0.9
    MacOS -> "bafybeigefhc7fmispumxmlbrsfsnjboujxgbynmzqvsqpoljqoclinqpke" -- 0.9
