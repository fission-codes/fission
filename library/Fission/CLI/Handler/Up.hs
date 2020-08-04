-- | File sync, IPFS-style
module Fission.CLI.Handler.Up (up) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import           System.FSNotify                 as FS

import           RIO.Directory

import           Network.HTTP.Types.Status

import           Network.IPFS
import qualified Network.IPFS.Add                as IPFS
import           Network.IPFS.CID.Types

import qualified Fission.Internal.UTF8           as UTF8
import           Fission.Prelude
import qualified Fission.Time                    as Time
import           Fission.URL

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token

import           Fission.Web.Client              as Client
import           Fission.Web.Client.App          as App
import           Fission.Web.Client.Error

import           Fission.CLI.Environment         as Environment

import           Fission.CLI.Display.Error
import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success

import qualified Fission.CLI.Prompt.BuildDir     as Prompt

import           Fission.CLI.Parser.Watch.Types

-- | Sync the current working directory to the server over IPFS
up ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  )
  => WatchFlag
  -> (m () -> IO ())
  -> URL
  -> FilePath
  -> Bool
  -> Bool
  -> m ()
up watchFlag runner appURL appPath _updateDNS updateData = do -- FIXME updateDNS
  ignoredFiles <- getIgnoredFiles
  toAdd        <- Prompt.checkBuildDir appPath
  absPath      <- liftIO $ makeAbsolute toAdd

  let copyFiles = updateData
  logDebug $ "Starting single IPFS add locally of " <> displayShow absPath

  IPFS.addDir ignoredFiles absPath >>= putErrOr \cid@(CID hash) -> do
    req <- App.mkUpdateReq appURL cid copyFiles

    retryOnStatus [status502, status504] 100 req >>= \case
      Left err ->
        CLI.Error.put err "Server unable to sync data"

      Right _ ->
        case watchFlag of
          WatchFlag False ->
            success appURL

          WatchFlag True ->
            liftIO $ FS.withManager \watchMgr -> do
              hashCache <- newMVar hash
              timeCache <- newMVar =<< getCurrentTime
              void $ handleTreeChanges runner appURL copyFiles timeCache hashCache watchMgr absPath
              forever . liftIO $ threadDelay 1_000_000 -- Sleep main thread

handleTreeChanges ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadTime      m
  , MonadLocalIPFS m
  , MonadWebClient m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  , ServerDID      m
  )
  => (m () -> IO ())
  -> URL
  -> Bool
  -> MVar UTCTime
  -> MVar Text
  -> WatchManager
  -> FilePath
  -> IO StopListening
handleTreeChanges runner appURL copyFilesFlag timeCache hashCache watchMgr dir =
  FS.watchTree watchMgr dir (\_ -> True) \_ -> runner do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    unless (diffUTCTime now oldTime < Time.doherty) do
      void $ swapMVar timeCache now
      threadDelay Time.dohertyMicroSeconds -- Wait for all events to fire in sliding window

      IPFS.addDir [] dir >>= \case
        Left err ->
          CLI.Error.put' err

        Right cid@(CID newHash) -> do
          oldHash <- swapMVar hashCache newHash
          logDebug $ "CID: " <> display oldHash <> " -> " <> display newHash

          unless (oldHash == newHash) do
            UTF8.putText "\n"
            req <- App.mkUpdateReq appURL cid copyFilesFlag
            retryOnStatus [status502, status504] 100 req >>= \case
              Left err -> CLI.Error.put err "Server unable to sync data"
              Right _  -> success appURL

success :: MonadIO m => URL -> m ()
success appURL = do
  CLI.Success.live
  CLI.Success.dnsUpdated appURL
