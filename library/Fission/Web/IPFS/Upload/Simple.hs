module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import           Database.Selda
import           Servant

import           Fission.Prelude
import           Fission.Web.Server
import qualified Fission.Web.Error        as Web.Err

import           Fission.User
import           Fission.User.CID.Mutation as UserCID

import           Network.IPFS.Local.Class
import qualified Network.IPFS.Types      as IPFS
import           Network.IPFS.File.Types as File
import qualified Network.IPFS.Add        as IPFS

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add
  :: ( MonadSelda     (RIO cfg)
     , MonadLocalIPFS (RIO cfg)
     , HasLogFunc          cfg
     )
  => User
  -> RIOServer         cfg API
add User { userID } (Serialized rawData) = IPFS.addRaw rawData >>= \case
  Right newCID -> do
    [newCID]
      |> UserCID.createX userID
      |> void

    return newCID

  Left err ->
    Web.Err.throw err
