module Fission.Web.API.IPFS.DAG.Upload.Types (Upload) where

import qualified Network.IPFS.CID.Types     as IPFS
import qualified Network.IPFS.File.Types    as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Upload
  =  Auth.HigherOrder
  --
  :> Summary "Pin an IPFS DAG structure"
  :> Description "Pin some data not associated to a user app or file system. We call these loose pins, likely to be deprecated."
  --
  :> ReqBody '[PlainText, OctetStream] File.Serialized
  --
  :> Post    '[PlainText, OctetStream] IPFS.CID
