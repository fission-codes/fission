module Fission.CLI.Environment.IPFS (binCidFor) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Prelude

import           Fission.CLI.Environment.OS as OS

binCidFor :: OS.Supported -> IPFS.CID
binCidFor =
  IPFS.CID . \case
    Linux -> "bafybeid3dhber24hcbo4gbm3kjap7gqbnfpdd53xbt6snovwx3x2y4ydry"
    NixOS -> "bafybeif526uqcfwqpgii7t4jghduisc4eaxfi64furk7g7e2amldqtf2m4"
    MacOS -> "bafybeih2yti3z7sw3hxpvhif2jpcanwttkeirfgfgeno5pgjjebdgymrtm"
