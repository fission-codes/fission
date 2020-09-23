module Fission.CLI.Environment.IPFS (binCidFor) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Prelude

import           Fission.CLI.Environment.OS as OS

binCidFor :: OS.Supported -> IPFS.CID
binCidFor =
  IPFS.CID . \case
    Linux -> "bafybeigqhqdtdjlzr6mh3qfxrbh2eot53ujlyeh5kv2mhesflllwdsmkyi" -- 0.7
    NixOS -> "bafybeif526uqcfwqpgii7t4jghduisc4eaxfi64furk7g7e2amldqtf2m4" -- FIXME 0.6 waiting on a 0.7  build
    MacOS -> "bafybeigefhc7fmispumxmlbrsfsnjboujxgbynmzqvsqpoljqoclinqpke" -- 0.7
