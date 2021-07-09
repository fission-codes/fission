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
import qualified RIO.Text                                      as Text

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
  status        <- liftIO $ newTVarIO . Right $ BytesReceived 0
  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) newCID (Just True)

  let (asyncRefs, chans) = NonEmpty.unzip pseudoStreams
  asyncListeners <- fanIn chans status

  _ <- liftIO ( pure |> withAsync do
    results <- waitAll asyncRefs
    when (all isLeft results) $ atomically do
      let
        result' =
          case NonEmpty.head results of
            Right PinStatus {progress} ->
              case progress of
                Nothing    -> Right $ BytesReceived 0
                Just bytes -> Right $ BytesReceived bytes

            Left err
              -> Left err

      writeTVar status result')

  Streamly.repeatM (readTVarIO status)
    |> Streamly.delay 0.500
    |> Streamly.takeWhile isRight
    |> Streamly.finally do
         forM_ asyncListeners cancel
         forM_ asyncRefs      cancel
         readTVarIO status
    |> Streamly.take 1
    |> Streamly.takeWhile isLeft
    |> asSerial
    |> toSourceIO
    |> mapStepT simplify
    |> pure

simplify :: Monad m => StepT m (Either ClientError BytesReceived) -> StepT m BytesReceived
simplify = \case
  S.Yield (Right byteCount) more -> S.Yield byteCount (simplify more)
  S.Yield (Left  err)       _    -> S.Error (show err)

  S.Skip   more                  -> simplify more
  S.Effect action                -> S.Effect $ fmap simplify action
  S.Error  msg                   -> S.Error msg
  S.Stop                         -> S.Stop

fanIn ::
  MonadIO m
  => NonEmpty (TChan (Either ClientError PinStatus))
  -> TVar (Either ClientError BytesReceived)
  -> m (NonEmpty (Async ()))
fanIn chans statusVar =
  forM chans \statusChan -> do
    liftIO $ withAsync (atomically $ reportBytes statusChan statusVar) pure

asSerial :: Streamly.Serial a -> Streamly.Serial a
asSerial a = a

reportBytes ::
  TChan (Either ClientError PinStatus)
  -> TVar (Either ClientError BytesReceived)
  -> STM ()
reportBytes channel status =
  readTVar status >>= \case
    Left _ ->
      return ()

    Right (BytesReceived lastMax) ->
      readTChan channel >>= \case
        Left _ ->
          return ()

        Right PinStatus {progress} ->
          case progress of
            Nothing ->
              return ()

            Just bytesHere -> do -- FIXME I think it's bytes? Maybe blocks?
              when (bytesHere > lastMax) do
                writeTVar status . Right $ BytesReceived bytesHere

              reportBytes channel status
