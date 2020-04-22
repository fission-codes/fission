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

import           Servant.Client

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS
import           Network.IPFS.CID.Types

import           Options.Applicative.Simple hiding (command)
import           System.FSNotify as FS

import           Fission.Prelude hiding (handle)
import qualified Fission.Time    as Time

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token
import           Fission.Web.Client as Client

import qualified Fission.Internal.UTF8        as UTF8
import qualified Fission.URL.DomainName.Types as URL

import           Fission.CLI.Display.Error as CLI.Error
import           Fission.CLI.Environment
 
import           Fission.CLI.Command.Types
import           Fission.CLI.Command.Watch.Types as Watch

import qualified Fission.CLI.DNS                 as CLI.DNS
import qualified Fission.CLI.IPFS.Pin            as CLI.Pin
import qualified Fission.CLI.Prompt.BuildDir     as Prompt

-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  , ServerDID      m
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
  , MonadTime      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  , ServerDID      m
  )
  => (m () -> IO ())
  -> Watch.Options
  -> m ()
watcher runner Watch.Options {..} = do
  ignoredFiles <- getIgnoredFiles
  toAdd        <- Prompt.checkBuildDir path
  absPath      <- makeAbsolute toAdd

  logDebug $ "Starting single IPFS add locally of " <> displayShow absPath

  IPFS.addDir ignoredFiles absPath >>= putErrOr \cid@(CID hash) -> do
    UTF8.putText $ "👀 Watching " <> Text.pack absPath <> " for changes...\n"

    when (not dnsOnly) do
      CLI.Pin.add cid >>= putErrOr \_ -> noop

    CLI.DNS.update cid >>= putErrOr \_ -> do
      liftIO $ FS.withManager \watchMgr -> do
        hashCache <- newMVar hash
        timeCache <- newMVar =<< getCurrentTime
        void $ handleTreeChanges runner timeCache hashCache watchMgr absPath
        forever . liftIO $ threadDelay 1000000 -- Sleep main thread

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
  -> MVar UTCTime
  -> MVar Text
  -> WatchManager
  -> FilePath
  -> IO StopListening
handleTreeChanges runner timeCache hashCache watchMgr dir =
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
            void $ pinAndUpdateDNS cid

pinAndUpdateDNS ::
  ( MonadUnliftIO  m
  , MonadTime m
  , MonadLogger    m
  , MonadWebClient m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  , ServerDID m
  )
  => CID
  -> m (Either ClientError URL.DomainName)
pinAndUpdateDNS cid =
  CLI.Pin.add cid >>= \case
    Left err -> do
      logError $ displayShow err
      return $ Left err

    Right _ ->
      CLI.DNS.update cid

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
