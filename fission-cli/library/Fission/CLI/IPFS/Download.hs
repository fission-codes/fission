module Fission.CLI.IPFS.Download
  ( getRelease
  , module Fission.CLI.IPFS.Download.GitHub.Types
  ) where

import qualified RIO.ByteString.Lazy                    as Lazy

import           Servant.Client

import           Fission.Prelude

import           Fission.CLI.IPFS.Download.GitHub.Types
import qualified Fission.CLI.IPFS.Release.Types         as IPFS

getRelease :: IPFS.Release -> ClientM Lazy.ByteString
getRelease release@IPFS.Release {..} = client (Proxy @GetRelease) version release
