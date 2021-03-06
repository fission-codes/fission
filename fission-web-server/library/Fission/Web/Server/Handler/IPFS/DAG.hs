module Fission.Web.Server.Handler.IPFS.DAG (put) where

import           Database.Esqueleto

import           Network.IPFS
import           Network.IPFS.Client.Streaming.Pin

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.DAG.Upload.Types  as API.DAG

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.LoosePin.Creator    as LoosePin
import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Server.IPFS.Client.DAG     as IPFS
import           Fission.Web.Server.IPFS.Cluster        as Cluster

put ::
  ( MonadRemoteIPFS    m
  , MonadIPFSCluster   m PinStatus
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => ServerT API.DAG.Upload m
put file Authorization {about = Entity userId _} = do
  IPFS.Response newCID <- Web.Err.ensureM $ IPFS.dagPut file
  _                    <- Web.Err.ensureM $ Cluster.pinStream newCID
  runDBNow $ LoosePin.createMany userId [newCID]
  return newCID
