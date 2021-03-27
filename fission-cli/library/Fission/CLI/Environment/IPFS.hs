module Fission.CLI.Environment.IPFS (binCidFor) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Prelude

import           Fission.CLI.Environment.OS as OS

binCidFor :: OS.Supported -> IPFS.CID
binCidFor =
  IPFS.CID . \case
    Linux -> "bafybeigqhqdtdjlzr6mh3qfxrbh2eot53ujlyeh5kv2mhesflllwdsmkyi" -- 0.7
    NixOS -> "bafybeicxpcyuibrrml2k4onfkbenqp35c7umguhizbnreptdy4ttxkb674" -- 0.7
    MacOS -> "bafybeigefhc7fmispumxmlbrsfsnjboujxgbynmzqvsqpoljqoclinqpke" -- 0.7
