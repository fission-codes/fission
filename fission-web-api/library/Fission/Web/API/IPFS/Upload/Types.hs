module Fission.Web.API.IPFS.Upload.Types (Upload) where

import qualified Network.IPFS.CID.Types     as IPFS
import qualified Network.IPFS.File.Types    as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Upload
  =  Auth.HigherOrder
  --
  :> Summary "Upload file"
  :> Description "Directly upload a file over HTTP"
  --
  :> ReqBody '[PlainText, OctetStream] File.Serialized
  :> Post    '[PlainText, OctetStream] IPFS.CID
