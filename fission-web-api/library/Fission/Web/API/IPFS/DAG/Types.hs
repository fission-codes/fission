module Fission.Web.API.IPFS.DAG.Types (Routes (..)) where

import qualified Network.IPFS.CID.Types     as IPFS
import qualified Network.IPFS.File.Types    as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

newtype Routes mode = Routes
  { upload ::
      mode
      :- Summary "Pin an IPFS DAG structure"
      :> Description "Pin some data not associated to a user app or file system. We call these loose pins, likely to be deprecated."
      --
      :> ReqBody '[PlainText, OctetStream] File.Serialized
      --
      :> Auth.HigherOrder
      :> Post '[PlainText, OctetStream] IPFS.CID
  }
  deriving Generic
