module Fission.CLI.IPFS.Download.GitHub.Types (GetRelease) where

import qualified RIO.ByteString.Lazy            as Lazy

import           Servant.API

import qualified Fission.CLI.IPFS.Release.Types as IPFS
import qualified Fission.CLI.IPFS.Version.Types as IPFS

type GetRelease
  = "ipfs"
  :> "kubo"
  :> "releases"
  :> "download"
  :> Capture "version" IPFS.Version
  :> Capture "release" IPFS.Release
  :> Get '[OctetStream] Lazy.ByteString
