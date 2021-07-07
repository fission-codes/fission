module Fission.Web.Server.Handler.App.Update (update) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.App.Update.Streaming.Types as API.App
import qualified Fission.Web.API.App.Update.Types           as API.App

import qualified Fission.Web.Server.App                     as App
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                   as Web.Error




-- 🌐

import qualified RIO.NonEmpty                               as NonEmpty

import           Servant.Types.SourceT
import qualified Streamly.Prelude                           as Streamly

import           Network.IPFS.Client.Streaming.Pin

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                        as IPFS
import           Network.IPFS.Client.Pin                    as Pin
import           Network.IPFS.Client.Streaming.Pin          as Pin

import           Servant.Client
import qualified Servant.Client.Streaming                   as Streaming

-- ⚛️

import           Fission.Prelude

import           Fission.Web.Async
import           Fission.Web.Server.IPFS.Cluster.Class






update :: (MonadLogger m, MonadThrow m, MonadTime m, App.Modifier m) => ServerT API.App.Update m
update url newCID copyDataFlag Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return ()
  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag

-- CID -> Authorization -> m (SourceT IO Natural)
updateStreaming ::
  ( MonadIO m
  , MonadLogger m
  , MonadThrow m
  , MonadTime m
  , App.Modifier m
  , MonadIPFSCluster m PinStatus
  )
  => ServerT (API.App.StreamingUpdate m) m
updateStreaming  url newCID Authorization {about = Entity userId _} = do
-- updateStreaming newCID Authorization {about = Entity userId _} = do
-- updateStreaming = do
  now <- currentTime

  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) undefined (Just True)
  -- pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) newCID (Just True)

  let
    asyncRefs = fst <$> pseudoStreams
    chans     = snd <$> pseudoStreams

  status       <- liftIO . newTVarIO $ Uploading 0
  asyncUpdates <- for chans \statusChan -> liftIO $ withAsync (action statusChan status) pure

--   status
--     |> readTVarIO
--     |> liftIO
--     |> Streamly.repeatM
--     |> Streamly.takeWhile isUploading
--     |> Streamly.finally (cancel <$> asyncUpdates)
--     |> Streamly.fromSerial
 --   |> toSourceIO

  return undefined --  (source [1] :: SourceIO Natural)

isUploading :: UploadStatus -> Bool
isUploading = \case
  Uploading _ -> True
  _           -> False

action :: MonadIO m => TChan (Either ClientError PinStatus) -> TVar UploadStatus -> m ()
action channel status =
  liftIO $ atomically go
  where
    go = do
      readTVar status >>= \case
        Done ->
          return () -- FIXME?

        Failed ->
          return () -- FIXME!

        Uploading lastMax ->
          readTChan channel >>= \case
            Left err ->
              undefined -- FIXME

            Right PinStatus {progress} ->
              case progress of
                Nothing ->
                  return ()

                Just bytesHere -> do -- FIXME I think it's bytes? Maybe blocks?
                  when (bytesHere > lastMax) do
                    writeTVar status $ Uploading bytesHere

                  go

data UploadStatus
  = Failed
  | Uploading Natural
  | Done

-- class ToSourceIO chunk a | a -> chunk where

instance ToSourceIO a (SerialT a) where
  -- toSourceIO :: SerialT a -> SourceIO a
  toSourceIO serialStream = do
    StreamT \k -> do
