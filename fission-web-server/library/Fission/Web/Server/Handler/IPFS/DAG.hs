module Fission.Web.Server.Handler.IPFS.DAG (handler) where

import           Database.Esqueleto

import           Network.IPFS
import qualified Network.IPFS.Client.DAG.Put.Types      as IPFS.DAG
import           Network.IPFS.Client.Streaming.Pin
import qualified Network.IPFS.DAG                       as IPFS.DAG

import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.DAG.Types         as DAG

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.LoosePin.Creator    as LoosePin
import           Fission.Web.Server.MonadDB

import           Fission.Web.Server.IPFS.Cluster        as Cluster

handler ::
  ( MonadRemoteIPFS    m
  , MonadIPFSCluster   m PinStatus
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => DAG.Routes (AsServerT m)
handler = DAG.Routes {..}
  where
    upload file Authorization {about = Entity userId _} = do
      IPFS.DAG.Response newCID <- Web.Err.ensureM $ IPFS.DAG.putRemote file
      _                        <- Web.Err.ensureM $ Cluster.pinStream newCID
      runDBNow $ LoosePin.createMany userId [newCID]
      return newCID
