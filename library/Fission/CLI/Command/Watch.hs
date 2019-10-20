-- | Continuous file sync
module Fission.CLI.Command.Watch
  ( command
  , handleTreeChanges
  , watcher
  ) where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text
import           RIO.Time

import           Data.Has

import           Options.Applicative.Simple hiding (command)
import           System.FSNotify as FS

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Web.Client   as Client
import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.Time         as Time

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Types as IPFS
import qualified Fission.AWS.Types  as AWS

import           Fission.Internal.Exception
import           Fission.CLI.Display.Error as CLI.Error

import qualified Fission.CLI.Auth          as Auth
import           Fission.CLI.Config.Types
import qualified Fission.CLI.Pin           as CLI.Pin
import qualified Fission.CLI.DNS           as CLI.DNS

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the IPFS network"
    (runRIO cfg . watcher)
    commandOptionsParser


data CommandOptions = CommandOptions
  { optDnsOnly :: Bool
  , optLocation :: String
  }


commandOptionsParser :: Parser CommandOptions
commandOptionsParser = do
  optDnsOnly <- dnsOnly
  optLocation <- location

  pure CommandOptions {..}

  where
    dnsOnly :: Parser Bool
    dnsOnly =
      [ long "dns-only"
      , help "Don't upload anything to Fission"
      ]
      & mconcat
      & flag False True

    location :: Parser String
    location =
      [ metavar "Location"
      , help    "The location of the assets you want to watch"
      , value   "./"
      ]
      & mconcat
      & strArgument


-- | Continuously sync the current working directory to the server over IPFS
watcher :: MonadRIO          cfg m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => CommandOptions
        -> m ()
watcher options = handleWith_ CLI.Error.put' do
  let dir = optLocation options
  let dnsOnly = optDnsOnly options

  cfg <- ask
  curr <- getCurrentDirectory

  let absDir = if isAbsolute dir then dir else curr </> dir
  UTF8.putText $ "👀 Watching " <> Text.pack dir <> " for changes...\n"

  -- Add directory to local IPFS
  cid@(CID hash) <- liftE $ IPFS.addDir absDir

  -- Pin the content on Fission if needed
  if dnsOnly then
    return ()
  else
    hash
      & CID
      & CLI.Pin.run
      & Auth.withAuth
      & liftE
      & fmap (\_ -> ())

  -- Update DNS
  liftE . Auth.withAuth $ CLI.DNS.update cid

  -- Watch for new changes
  liftIO $ FS.withManager \watchMgr -> do
    hashCache <- newMVar hash
    timeCache <- newMVar =<< getCurrentTime
    void $ handleTreeChanges timeCache hashCache watchMgr cfg absDir
    forever $ liftIO $ threadDelay 1000000 -- Sleep main thread

handleTreeChanges :: HasLogFunc        cfg
                  => Has Client.Runner cfg
                  => HasProcessContext cfg
                  => Has IPFS.BinPath  cfg
                  => Has IPFS.Timeout  cfg
                  => MVar UTCTime
                  -> MVar Text
                  -> WatchManager
                  -> cfg
                  -> FilePath
                  -> IO StopListening
handleTreeChanges timeCache hashCache watchMgr cfg dir =
  FS.watchTree watchMgr dir (const True) \_ -> runRIO cfg do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    if diffUTCTime now oldTime < Time.doherty
      then
        logDebug "Fired within change lock window"

      else do
        void $ swapMVar timeCache now
        threadDelay Time.dohertyMicroSeconds -- Wait for all events to fire in sliding window
        IPFS.addDir dir >>= \case
          Left err ->
            CLI.Error.put' err

          Right cid@(CID newHash) -> do
            oldHash <- swapMVar hashCache newHash
            logDebug $ "CID: " <> display oldHash <> " -> " <> display newHash
            when (oldHash /= newHash) . void $ pinAndUpdateDNS cid

pinAndUpdateDNS :: MonadRIO          cfg m
                => HasLogFunc        cfg
                => Has Client.Runner cfg
                => HasProcessContext cfg
                => Has IPFS.BinPath  cfg
                => Has IPFS.Timeout  cfg
                => CID
                -> m (Either SomeException AWS.DomainName)
pinAndUpdateDNS cid =
  Auth.withAuth (CLI.Pin.run cid) >>= \case
    Left err -> do
      logError $ displayShow err
      return $ Left err

    Right _ ->
      Auth.withAuth $ CLI.DNS.update cid
