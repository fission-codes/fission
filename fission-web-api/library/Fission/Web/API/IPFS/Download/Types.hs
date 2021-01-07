module Fission.Web.API.IPFS.Download.Types
  ( Download
  , ViaQuery
  , ViaPath
  ) where

import qualified Network.IPFS.CID.Types  as IPFS
import qualified Network.IPFS.File.Types as File

import           Fission.Web.API.Prelude

type Download = ViaPath :<|> ViaQuery

type ViaPath
  =  Summary "Get a file (path)"
  :> Description "Download a file by its CID"
  --
  :> Capture "cid" IPFS.CID
  --
  :> Get '[OctetStream, PlainText] File.Serialized

type ViaQuery
  =  Summary "Get a file (query param)"
  :> Description "Download a file by its CID"
  --
  :> QueryParam "cid" IPFS.CID
  --
  :> Get '[OctetStream, PlainText] File.Serialized
