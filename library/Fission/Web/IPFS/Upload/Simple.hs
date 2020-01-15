module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import qualified Network.IPFS.Pin        as IPFS.Pin
import qualified Network.IPFS.Add        as IPFS
import qualified Network.IPFS.Types      as IPFS
import           Network.IPFS.File.Types as File

import           Fission.Models
import           Fission.Prelude

import           Fission.User.CID.Creator as User.CID
import qualified Fission.Web.Error        as Web.Err

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add ::
  ( MonadLocalIPFS     m
  , MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , User.CID.Creator t
  )
  => Entity User
  -> ServerT API m
add (Entity userId _) (Serialized rawData) = IPFS.addRaw rawData >>= \case
  Right newCID -> IPFS.Pin.add newCID >>= \case
    Right pinnedCID -> do
      pinnedCID
        |> pure
        |> User.CID.createX userId
        |> runDBNow

      return pinnedCID

    Left err ->
      Web.Err.throw err

  Left err ->
    Web.Err.throw err
