-- | Continuous file sync
module Fission.CLI.Command.Watch
  ( cmd
  , handleTreeChanges
  , watcher
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.Function

import           RIO.Directory
import qualified RIO.Text as Text

import           Servant.API.ContentTypes
import           Servant.Client

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS
import           Network.IPFS.CID.Types

import           Options.Applicative.Simple hiding (command)
import           System.FSNotify as FS

import           Fission.Prelude hiding (handle)
import           Fission.Error

import           Fission.URL
import           Fission.Models
import qualified Fission.Time                 as Time
import qualified Fission.Internal.UTF8        as UTF8

import           Fission.Authorization.ServerDID

import           Fission.Web.Client.App as App
import           Fission.Web.Auth.Token
import           Fission.Web.Client as Client

import           Fission.CLI.Display.Error as CLI.Error
import           Fission.CLI.Environment   as Environment

import qualified Fission.CLI.Command.App.Init as App.Init
 
import           Fission.CLI.Command.Types
import           Fission.CLI.Command.Watch.Types as Watch

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Prompt.BuildDir as Prompt


-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  )
  => (m () -> IO ())
  -> Command m Watch.Options ()
cmd runner = Command
  { command     = "watch"
  , description = "Keep your working directory in sync with the IPFS network"
  , argParser   = parseOptions
  , handler     = watcher runner
  }

-- | Continuously sync the current working directory to the server over IPFS
watcher ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  )
  => (m () -> IO ())
  -> Watch.Options
  -> m ()
watcher runner Watch.Options {..} = do
  ignoredFiles <- getIgnoredFiles
  toAdd        <- Prompt.checkBuildDir path
  absPath      <- liftIO $ makeAbsolute toAdd

  let copyFiles = not dnsOnly
 
  Environment {appURL} <- Environment.get

  case appURL of
    Nothing ->
      CLI.Error.put (NotFound @App) $
        "You have not set up an app. Please run " <> App.Init.cmdTxt

    Just url -> do
      logDebug $ "Starting single IPFS add locally of " <> displayShow absPath

      IPFS.addDir ignoredFiles absPath >>= putErrOr \cid@(CID hash) -> do
        UTF8.putText $ "👀 Watching " <> Text.pack absPath <> " for changes...\n"

        withRetryM 100 (sendRequestM (updateApp url cid copyFiles)) >>= putErrOr \_ -> do
          liftIO $ FS.withManager \watchMgr -> do
            hashCache <- newMVar hash
            timeCache <- newMVar =<< getCurrentTime
            void $ handleTreeChanges runner url copyFiles timeCache hashCache watchMgr absPath
            forever . liftIO $ threadDelay 1000000 -- Sleep main thread

updateApp ::
  ( MonadIO      m
  , MonadTime    m
  , MonadLogger  m
  , ServerDID    m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => URL
  -> CID
  -> Bool
  -> m (ClientM NoContent)
updateApp url cid copyFiles =
  authClient (Proxy @App.Update)
    `withPayload` url
    `withPayload` cid
    `withPayload` (Just copyFiles)

handleTreeChanges ::
  ( MonadUnliftIO  m
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
  FS.watchTree watchMgr dir (const True) \_ -> runner do
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
            sendRequestM (updateApp appURL cid copyFilesFlag) >>= \case
              Left err ->
                CLI.Error.put err "Server unable to sync data"

              Right _  -> do
                CLI.Success.live cid
                CLI.Success.dnsUpdated appURL

parseOptions :: Parser Watch.Options
parseOptions = do
  dnsOnly <- switch $ mconcat
    [ long "dns-only"
    , help "Only update DNS (i.e. don't actively sync files to the server)"
    ]

  path <- strArgument $ mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to watch"
    , value   "./"
    ]

  return Watch.Options {..}
