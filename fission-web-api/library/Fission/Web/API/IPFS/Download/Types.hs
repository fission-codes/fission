module Fission.Web.API.IPFS.Download.Types (Routes (..)) where

import qualified Network.IPFS.CID.Types  as IPFS
import qualified Network.IPFS.File.Types as File

import           Fission.Web.API.Prelude

data Routes mode = Routes
  { viaPath ::
      mode
      :- Summary "Get a file (path)"
      :> Description "Download a file by its CID"
      --
      :> Capture "cid" IPFS.CID
      --
      :> Get '[OctetStream, PlainText] File.Serialized

  , viaQuery ::
      mode
      :- Summary "Get a file (query param)"
      :> Description "Download a file by its CID"
      --
      :> QueryParam "cid" IPFS.CID
      --
      :> Get '[OctetStream, PlainText] File.Serialized
  }
  deriving Generic
