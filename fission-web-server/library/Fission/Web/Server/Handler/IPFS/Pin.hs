module Fission.Web.Server.Handler.IPFS.Pin
  ( handler
  , pin
  , unpin
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Pin                       as IPFS.Pin

import           Fission.Prelude

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Models

import qualified Fission.Web.Server.Error               as Web.Err
import qualified Fission.Web.Server.LoosePin            as LoosePin

handler ::
  ( MonadRemoteIPFS      m
  , MonadLogger          m
  , MonadThrow           m
  , MonadTime            m
  , MonadDB            t m
  , LoosePin.Creator   t
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userId _} = pin userId :<|> unpin userId

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
