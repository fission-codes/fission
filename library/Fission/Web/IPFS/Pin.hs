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
import qualified Network.IPFS.Pin   as IPFS.Pin
import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.Web.Error as Web.Err
import qualified Fission.LoosePin  as LoosePin
import           Fission.Models

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
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
  => Entity User
  -> ServerT API m
server (Entity userId _) = pin userId :<|> unpin userId

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
    cid
      |> LoosePin.create userId
      |> runDBNow

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

  when (remaining == []) do
    void <| Web.Err.ensure =<< IPFS.Pin.rm cid

  return NoContent
