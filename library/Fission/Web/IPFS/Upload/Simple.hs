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
import           Fission.User.CID.Mutation as User.CID

import           Network.IPFS
import qualified Network.IPFS.Types      as IPFS
import           Network.IPFS.File.Types as File
import qualified Network.IPFS.Add        as IPFS
import qualified Network.IPFS.Pin        as IPFS.Pin

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add ::
  ( MonadSelda      (RIO cfg)
  , MonadLocalIPFS  (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , HasLogFunc           cfg
  )
  => User
  -> RIOServer         cfg API
add User { userID } (Serialized rawData) = IPFS.addRaw rawData >>= \case
  Right newCID -> IPFS.Pin.add newCID >>= \case
    Right pinnedCID -> do
      _ <- User.CID.createX userID [pinnedCID]
      return pinnedCID

    Left err ->
      Web.Err.throw err

  Left err ->
    Web.Err.throw err
