module Fission.Web.Server.Handler.IPFS.Pin
  ( handler
  , pin
  , unpin
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import qualified Network.IPFS.Pin                       as IPFS.Pin

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Pin.Create.Types  as API.IPFS.Pin
import qualified Fission.Web.API.IPFS.Pin.Destroy.Types as API.IPFS.Pin
import qualified Fission.Web.API.IPFS.Pin.Types         as API.IPFS

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import qualified Fission.Web.Server.LoosePin            as LoosePin
import           Fission.Web.Server.MonadDB

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
  => ServerT API.IPFS.Pin m
handler = pin :<|> unpin

pin ::
  ( MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => ServerT API.IPFS.Pin.Create m
pin cid Authorization {about = Entity userId _} = IPFS.Pin.add cid >>= \case
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
  => ServerT API.IPFS.Pin.Destroy m
unpin cid Authorization {about = Entity userId _} = do
  remaining <- runDB do
    LoosePin.destroy userId cid
    LoosePin.getByCids [cid]

  when (null remaining) do
    void . Web.Err.ensureM $ IPFS.Pin.rm cid

  return NoContent
