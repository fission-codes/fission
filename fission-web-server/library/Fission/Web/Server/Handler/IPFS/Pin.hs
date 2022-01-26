module Fission.Web.Server.Handler.IPFS.Pin (handler) where

import           Database.Esqueleto.Legacy

import           Servant
import           Servant.Server.Generic

import           Network.IPFS
import qualified Network.IPFS.Pin                       as IPFS.Pin

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Pin.Types         as IPFS.Pin

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
  => IPFS.Pin.Routes (AsServerT m)
handler = IPFS.Pin.Routes {..}
  where
    pin cid Authorization {about = Entity userId _} = IPFS.Pin.add cid >>= \case
      Left err -> Web.Err.throw err
      Right _  -> do
        runDBNow $ LoosePin.create userId cid
        pure NoContent

    unpin cid Authorization {about = Entity userId _} = do
      remaining <- runDB do
        LoosePin.destroy userId cid
        LoosePin.getByCids [cid]

      when (null remaining) do
        void . Web.Err.ensureM $ IPFS.Pin.rm cid

      return NoContent
