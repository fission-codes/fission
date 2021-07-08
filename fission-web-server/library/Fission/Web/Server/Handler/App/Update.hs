module Fission.Web.Server.Handler.App.Update (update) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.App.Update.Streaming.Types as API.App
import qualified Fission.Web.API.App.Update.Types           as API.App

import qualified Fission.Web.Server.App                     as App
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                   as Web.Error




-- 🌐

import qualified RIO.List                                   as List
import qualified RIO.NonEmpty                               as NonEmpty

import           Servant.Types.SourceT                      as S
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

import           Fission.URL.Types




import           Fission.Web.API.App.Update.Streaming.Types


update :: (MonadLogger m, MonadThrow m, MonadTime m, App.Modifier m) => ServerT API.App.Update m
update url newCID copyDataFlag Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return ()
  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag

instance ToSourceIO a (Streamly.SerialT IO a) where
  toSourceIO serialStream =
    SourceT \k ->
      k $ Effect do
        cont <- Streamly.foldr folder Skip serialStream
        return $ cont Stop
    where
      folder :: a -> (StepT IO a -> StepT IO a) -> (StepT IO a -> StepT IO a)
      folder x contAcc = \nextCont -> contAcc (Yield x nextCont)

-- CID -> Authorization -> m (SourceT IO Natural)
updateStreaming ::
  forall m .
  ( MonadIO m
  , MonadLogger m
  , MonadThrow m
  , MonadTime m
  , MonadBaseControl IO m
  , App.Modifier m
  , MonadIPFSCluster m PinStatus
  , ServerT API.App.StreamingUpdate m ~ (URL -> CID -> Authorization -> m (SourceT IO UploadStatus))
  )
  => ServerT API.App.StreamingUpdate m
updateStreaming  url newCID Authorization {about = Entity userId _} = do
  now           <- currentTime
  status        <- liftIO . newTVarIO $ Uploading 0 -- FIXME switch to mvar?
  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) newCID (Just True)

  let
    (asyncRefs, chans) = NonEmpty.unzip pseudoStreams

  asyncUpdates <- for chans \statusChan -> liftIO $ withAsync (action statusChan status) pure

  let
    source :: Streamly.SerialT IO UploadStatus
    source = Streamly.repeatM (readTVarIO status)

  source
    |> Streamly.takeWhile isUploading
    |> Streamly.finally (forM asyncRefs cancel)
    |> toSourceIO
    |> pure

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

toSourceT :: Monad m => Streamly.SerialT m a -> SourceT m a
toSourceT serialStream =
  SourceT \k ->
    k $ Effect do
      cont <- Streamly.foldr folder Skip serialStream
      return $ cont Stop
  where
    folder :: a -> (StepT m a -> StepT m a) -> (StepT m a -> StepT m a)
    folder x contAcc = \nextCont -> contAcc (Yield x nextCont)
