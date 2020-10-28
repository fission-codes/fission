module Fission.Web.IPFS.Pin
  ( API
  , PinAPI
  , UnpinAPI
  , server
  , pin
  , unpin
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Pin                           as IPFS.Pin

import           Fission.Prelude

import qualified Fission.Authorization                      as Authorization
import           Fission.Models
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.LoosePin                           as LoosePin
import qualified Fission.Web.Error                          as Web.Err

type API = PinAPI :<|> UnpinAPI

type PinAPI
  =  Summary "Pin CID"
  :> Description "DEPRECATED ⛔ Pin an otherwise unassociated CID"
  :> Capture "cid" CID
  :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI
  =  Summary "Unpin CID"
  :> Description "DEPRECATED ⛔ Unpin an otherwise unassociated CID"
  :> Capture "cid" CID
  :> DeleteAccepted '[PlainText, OctetStream] NoContent

server ::
  ( MonadRemoteIPFS      m
  , MonadLogger          m
  , MonadThrow           m
  , MonadTime            m
  , MonadDB            t m
  , LoosePin.Creator   t
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => Authorization.Session
  -> ServerT API m
-- server Authorization {about = Entity userId _} = pin userId :<|> unpin userId
server Authorization.Session {} = do
  let userId = undefined -- FIXME
  pin userId :<|> unpin userId

pin ::
  ( MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => UserId
  -> ServerT PinAPI m
pin userId cid = IPFS.Pin.add cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> do
    runDBNow $ LoosePin.create userId cid
    pure NoContent

unpin ::
  ( MonadRemoteIPFS      m
  , MonadLogger          m
  , MonadThrow           m
  , MonadDB            t m
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => UserId
  -> ServerT UnpinAPI m
unpin userId cid = do
  remaining <- runDB do
    LoosePin.destroy userId cid
    LoosePin.getByCids [cid]

  when (null remaining) do
    void . Web.Err.ensureM $ IPFS.Pin.rm cid

  return NoContent
