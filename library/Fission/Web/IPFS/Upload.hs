module Fission.Web.IPFS.Upload
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

import           Fission.Prelude
import           Fission.Authorization.Types

import           Fission.LoosePin.Creator as LoosePin
import qualified Fission.Web.Error        as Web.Err

type API
  =  Summary "Upload file"
  :> Description "Directly upload a file over HTTP"
  :> ReqBody '[PlainText, OctetStream] File.Serialized
  :> Post    '[PlainText, OctetStream] IPFS.CID

add ::
  ( MonadLocalIPFS     m
  , MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => Authorization
  -> ServerT API m
add Authorization {about = Entity userId _} (Serialized rawData) =
  IPFS.addRaw rawData >>= \case
    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Right pinnedCID -> do
          runDBNow $ LoosePin.createMany userId [pinnedCID]
          return pinnedCID

        Left err ->
          Web.Err.throw err

    Left err ->
      Web.Err.throw err
