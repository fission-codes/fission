-- | File sync, IPFS-style
module Fission.CLI.Handler.App.Publish (publish) where

import qualified Data.Yaml                                 as YAML

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import           System.FSNotify                           as FS

import           RIO.Directory
import qualified RIO.Text                                  as Text
import           Web.Browser

import           Network.HTTP.Types.Status
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Network.IPFS
import           Network.IPFS.CID.Types

import           Fission.Prelude

import qualified Fission.Process.Time                      as Process

import qualified Fission.Web.Client.App                    as App

import qualified Fission.Internal.UTF8                     as UTF8

import qualified Fission.Time                              as Time
import           Fission.URL

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import           Fission.Web.Auth.Token.JWT                as JWT
import           Fission.Web.Auth.Token.Types

import           Fission.Web.Client                        as Client
import           Fission.Web.Client.Error

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success

import qualified Fission.CLI.IPFS.Add                      as CLI.IPFS.Add
import           Fission.CLI.IPFS.Daemon                   as IPFS.Daemon

import           Fission.CLI.App.Environment               as App
import           Fission.CLI.Parser.Open.Types
import           Fission.CLI.Parser.Watch.Types

import           Fission.CLI.Environment                   (MonadEnvironment)
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

-- | Sync the current working directory to the server over IPFS
publish ::
  ( MonadIO          m
  , MonadCleanup     m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadIPFSDaemon  m
  , UCAN.MonadStore  m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  , m `Raises` YAML.ParseException
  , m `Raises` ClientError
  , m `Raises` IPFS.Process.Error
  , m `Raises` NotFound FilePath
  , Show (OpenUnion (Errors m))
  , CheckErrors m
  )
  => OpenFlag
  -> WatchFlag
  -> (m () -> IO ())
  -> URL
  -> FilePath
  -> Bool
  -> Bool
  -> m ()
publish openFlag watchFlag runner appURL appPath _updateDNS updateData = do -- TODO updateDNS
  logDebug @Text "📱 App publish"
  attempt (App.readFrom appPath) >>= \case
    Left err -> do
      CLI.Error.put err "App not set up. Please double check your path, or run `fission app register`"
      raise err

    Right App.Env {buildDir} -> do
      absBuildPath <- liftIO $ makeAbsolute buildDir
      logDebug $ "📱 Starting single IPFS add locally of " <> displayShow absBuildPath
      logUser @Text "🛫 App publish local preflight"

      CLI.IPFS.Add.dir absBuildPath >>= \case
        Left err -> do
          CLI.Error.put' err
          raise err

        Right cid@(CID hash) -> do
          logDebug $ "📱 Directory CID is " <> hash
          _     <- IPFS.Daemon.runDaemon
          proof <- getRootUserProof
          req   <- App.update appURL cid (Just updateData) <$> Client.attachAuth proof

          logUser @Text "✈️  Pushing to remote"
          retryOnStatus [status502] 100 req >>= \case
            Left err -> do
              CLI.Error.put err "Server unable to sync data"
              raise err

            Right _ -> do
              let watch =
                    liftIO $ FS.withManager \watchMgr -> do
                      hashCache <- newMVar hash
                      timeCache <- newMVar =<< getCurrentTime
                      void $ handleTreeChanges runner proof appURL updateData timeCache hashCache watchMgr absBuildPath
                      forever . liftIO $ threadDelay 1_000_000 -- Sleep main thread

              let open =
                    liftIO . openBrowser $ "https://ipfs.runfission.com/ipns/" <> Text.unpack (textDisplay appURL)

              case (openFlag, watchFlag) of
                (OpenFlag True, WatchFlag True) ->
                  open >> watch

                (OpenFlag True, _) ->
                  open >> success appURL

                (_, WatchFlag True) -> watch

                (OpenFlag False, WatchFlag False) ->
                  success appURL

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
  -> JWT.Proof
  -> URL
  -> Bool
  -> MVar UTCTime
  -> MVar Text
  -> WatchManager
  -> FilePath -- ^ Build dir
  -> IO StopListening
handleTreeChanges runner userProof appURL copyFilesFlag timeCache hashCache watchMgr absDir =
  FS.watchTree watchMgr absDir (\_ -> True) \_ -> runner do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    unless (diffUTCTime now oldTime < Time.doherty) do
      void $ swapMVar timeCache now
      Process.sleepThread Time.dohertyMicroSeconds

      CLI.IPFS.Add.dir absDir >>= \case
        Left err ->
          CLI.Error.put' err

        Right cid@(CID newHash) -> do
          oldHash <- swapMVar hashCache newHash
          logDebug $ "CID: " <> display oldHash <> " -> " <> display newHash

          unless (oldHash == newHash) do
            UTF8.putText "\n"
---            req <- App.mkUpdateReq appURL cid copyFilesFlag

            req <- App.update appURL cid (Just copyFilesFlag) <$> attachAuth userProof

            retryOnStatus [status502] 100 req >>= \case
              Left err -> CLI.Error.put err "Server unable to sync data"
              Right _  -> success appURL

success :: MonadIO m => URL -> m ()
success appURL = do
  CLI.Success.live
  CLI.Success.dnsUpdated appURL
