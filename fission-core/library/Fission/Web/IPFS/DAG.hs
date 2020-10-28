module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import qualified Fission.Authorization                      as Authorization
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.LoosePin.Creator                   as LoosePin
import qualified Fission.Web.Error                          as Web.Err

import           Network.IPFS
import qualified Network.IPFS.DAG                           as IPFS.DAG
import           Network.IPFS.File.Types                    as File
import qualified Network.IPFS.Pin                           as IPFS.Pin
import qualified Network.IPFS.Types                         as IPFS

type API
  =  Summary "Pin an IPFS DAG structure"
  :> Description "Pin some data not associated to a user app or file system. We call these loose pins, likely to be deprecated."
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
  => Authorization.Session
  -> ServerT API m
put Authorization.Session {} (Serialized rawData) =
-- put Authorization {about = Entity userId _} (Serialized rawData) =
  IPFS.DAG.put rawData >>= \case
    Left err ->
      Web.Err.throw err

    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Left err ->
          Web.Err.throw err

        Right pinnedCID -> do
          let userId = undefined -- FIXME
          newCID
            |> return
            |> LoosePin.createMany userId
            |> runDBNow

          return pinnedCID
