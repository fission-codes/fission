module Fission.Web.Server.Handler.IPFS (handlerV_, handlerV2) where

import           Network.IPFS
import           Network.IPFS.Client.Streaming.Pin
import           Network.IPFS.File.Types                  as File
import           Network.IPFS.Remote.Class                as IPFS

import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Types               as IPFS

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error                 as Web.Err
import           Fission.Web.Server.LoosePin.Creator      as LoosePin
import           Fission.Web.Server.MonadDB

import           Fission.Web.Server.IPFS.Cluster
import           Fission.Web.Server.IPFS.Linked
import qualified Fission.Web.Server.LoosePin              as LoosePin

import qualified Fission.Web.Server.Handler.IPFS.CID      as CID
import qualified Fission.Web.Server.Handler.IPFS.DAG      as DAG
import qualified Fission.Web.Server.Handler.IPFS.Download as Download
import qualified Fission.Web.Server.Handler.IPFS.Peer     as Peer
import qualified Fission.Web.Server.Handler.IPFS.Pin      as Pin

handlerV2 :: MonadLinkedIPFS m => IPFS.RoutesV2 (AsServerT m)
handlerV2 = IPFS.RoutesV2 {peers = genericServerT Peer.handler}

handlerV_ ::
  ( MonadRemoteIPFS      m
  , MonadLinkedIPFS      m
  , MonadIPFSCluster     m PinStatus
  , MonadLogger          m
  , MonadThrow           m
  , MonadTime            m
  , MonadDB            t m
  , LoosePin.Creator   t
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => IPFS.RoutesV_ (AsServerT m)
handlerV_ =
  IPFS.RoutesV_
    { cid      = genericServerT CID.handler
    , dag      = genericServerT DAG.handler
    , peers    = genericServerT Peer.handler
    , pin      = genericServerT Pin.handler
    , download = genericServerT Download.handler
    , upload
    }
  where
    upload (Serialized rawData) Authorization {about = Entity userId _} =
      IPFS.ipfsAdd rawData >>= \case
        Left err ->
          Web.Err.throw err

        Right newCID ->
          IPFS.ipfsPin newCID >>= \case
            Right _ -> do
              runDBNow $ LoosePin.createMany userId [newCID]
              return newCID

            Left err ->
              Web.Err.throw err
