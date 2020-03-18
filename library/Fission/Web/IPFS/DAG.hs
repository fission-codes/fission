module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Database.Esqueleto
import           Servant

import           Fission.Models
import           Fission.Prelude

import           Fission.LoosePin.Creator as LoosePin
import qualified Fission.Web.Error        as Web.Err

import           Network.IPFS
import           Network.IPFS.File.Types as File
import qualified Network.IPFS.Types      as IPFS
import qualified Network.IPFS.DAG        as IPFS.DAG
import qualified Network.IPFS.Pin        as IPFS.Pin

type API
  =  Summary "Upload otherwise unassociated data"
  :> ReqBody '[PlainText, OctetStream] File.Serialized
  :> Post    '[PlainText, OctetStream] IPFS.CID

put ::
  ( MonadRemoteIPFS    m
  , MonadLocalIPFS     m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => Entity User
  -> ServerT API m
put (Entity userId _) (Serialized rawData) =
  IPFS.DAG.put rawData >>= \case
    Left err ->
      Web.Err.throw err

    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Left err ->
          Web.Err.throw err

        Right pinnedCID -> do
          newCID
            |> return
            |> LoosePin.createMany userId
            |> runDBNow

          return pinnedCID
