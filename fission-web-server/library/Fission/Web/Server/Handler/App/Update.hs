module Fission.Web.Server.Handler.App.Update (update, updateStreaming) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.App.Update.Streaming.Types    as API.App
import qualified Fission.Web.API.App.Update.Types              as API.App

import qualified Fission.Web.Server.App                        as App
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                      as Web.Error




-- 🌐

import qualified RIO.List                                      as List
import qualified RIO.NonEmpty                                  as NonEmpty

import           Servant.Types.SourceT                         as S
import qualified Streamly.Prelude                              as Streamly

import           Network.IPFS.Client.Streaming.Pin

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                           as IPFS
import           Network.IPFS.Client.Pin                       as Pin
import           Network.IPFS.Client.Streaming.Pin             as Pin

import           Servant.Client
import qualified Servant.Client.Streaming                      as Streaming

-- ⚛️

import           Fission.Prelude

import           Fission.Web.Async
import           Fission.Web.Server.IPFS.Cluster.Class

import           Fission.URL.Types

import           Fission.Process.Time
import           Fission.Time



import           Fission.Web.API.App.Update.Streaming.Types


import           Fission.Web.Server.Internal.Orphanage.SerialT ()

update :: (MonadLogger m, MonadThrow m, MonadTime m, App.Modifier m) => ServerT API.App.Update m
update url newCID copyDataFlag Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return ()
  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag

updateStreaming ::
  ( MonadIO m
  , MonadLogger m
  , MonadThrow m
  , MonadTime m
  , App.Modifier m
  , MonadIPFSCluster m PinStatus
  )
  => ServerT API.App.StreamingUpdate m
updateStreaming  url newCID Authorization {about = Entity userId _} = do
  now           <- currentTime
  status        <- liftIO . newTVarIO $ Uploading 0 -- FIXME switch to mvar?
  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) newCID (Just True)

  let (asyncRefs, chans) = NonEmpty.unzip pseudoStreams
  asyncListeners <- foo chans status

  let
    source :: Streamly.SerialT IO (Maybe BytesReceived)
    source =
      Streamly.repeatM do
        sleepThread . Seconds $ Milli @Natural 500
        readTVarIO status >>= \case
          Uploading byteCount -> return . Just $ BytesReceived byteCount
          _                   -> return $ Nothing

  source
    |> Streamly.takeWhile isJust
    |> Streamly.finally do
         forM_ asyncListeners cancel
         forM_ asyncRefs      cancel
    |> toSourceIO
    |> mapStepT go
    |> pure

  where
    go :: Monad m => StepT m (Maybe BytesReceived) -> StepT m BytesReceived
    go = \case
      S.Yield Nothing          more -> go more
      S.Yield (Just byteCount) more -> S.Yield byteCount (go more)

      S.Skip   more                 -> go more
      S.Effect action               -> S.Effect $ fmap go action
      S.Error  msg                  -> S.Error msg
      S.Stop                        -> S.Stop

foo ::
  MonadIO m
  => NonEmpty (TChan (Either ClientError PinStatus))
  -> TVar UploadStatus
  -> m (NonEmpty (Async ()))
foo chans statusVar =
  forM chans \statusChan ->
    liftIO $ withAsync (atomically $ fanIn statusChan statusVar) pure

fanIn :: TChan (Either ClientError PinStatus) -> TVar UploadStatus -> STM ()
fanIn channel status = go
  where
    go :: STM ()
    go =
      readTVar status >>= \case
        Done ->
          return ()

        Failed ->
          return ()

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
