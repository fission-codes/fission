{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.CLI.Types
  ( FissionCLI (..)
  , runFissionCLI
  ) where

import           Crypto.Hash                             as Crypto
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import           Crypto.Random

import qualified Data.Yaml                               as YAML

import           Control.Monad.Catch                     as Catch

import qualified RIO.ByteString.Lazy                     as Lazy
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                                as Text

import qualified Network.DNS                             as DNS
import           Network.HTTP.Client                     as HTTP hiding (Proxy)
import           Network.IPFS                            as IPFS
import qualified Network.IPFS.Process.Error              as Process
import           Network.IPFS.Types                      as IPFS

import           Servant.Client

import qualified Turtle

import           Fission.Prelude                         hiding (mask,
                                                          uninterruptibleMask)

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import qualified Fission.Key.Error                       as Key
import           Fission.User.DID.Types
import           Fission.Web.Client.HTTP.Class

import qualified Fission.CLI.Base.Types                  as Base
import           Fission.CLI.Bootstrap
import qualified Fission.CLI.Connected.Types             as Connected

import           Fission.CLI.IPFS.Daemon                 as IPFS.Daemon
import           Fission.CLI.IPFS.Ignore                 as IPFS.Ignore

import           Fission.CLI.Key.Store                   as Key.Store

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.Bearer.Types     as Bearer
import           Fission.Web.Auth.Token.JWT              as JWT

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT                  as JWT

import qualified Fission.CLI.Display.Loader              as CLI
import           Fission.CLI.Environment
import           Fission.CLI.Environment.Path

import           Fission.Internal.Orphanage.BaseUrl      ()
import           Fission.Internal.Orphanage.ClientError  ()
import           Fission.Internal.Orphanage.DNS.DNSError ()
import           Fission.Internal.Orphanage.OpenUnion    ()

newtype FissionCLI errs cfg a = FissionCLI
  { unFissionCLI :: RescueT errs (RIO cfg) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader cfg
                   , MonadThrow
                   , MonadCatch
                   )

runFissionCLI :: forall errs m cfg a .
  MonadIO m
  => cfg
  -> FissionCLI errs cfg a
  -> m (Either (OpenUnion errs) a)
runFissionCLI cfg = runRIO cfg . runRescueT . unFissionCLI

instance forall errs cfg.
  ( Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  => MonadRaise (FissionCLI errs cfg) where
  type Errors (FissionCLI errs cfg) = errs

  raise err = do
    logDebug $ "Raised exception: " <> display (include err :: OpenUnion errs)
    FissionCLI $ raise err

instance
  ( HasLogFunc cfg
  , Display (OpenUnion errs)
  )
  => MonadRescue (FissionCLI errs cfg) where
  attempt (FissionCLI (RescueT action)) =
    FissionCLI . RescueT $ Right <$> action

instance HasLogFunc cfg => MonadLogger (FissionCLI errs cfg) where
  monadLoggerLog loc src lvl msg =
    FissionCLI (RescueT (Right <$> monadLoggerLog loc src lvl msg))

instance
  ( Contains errs errs
  , Display (OpenUnion errs)
  , IsMember SomeException errs
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "fissionURL"  cfg BaseUrl
  , HasLogFunc              cfg
  )
  => MonadWebClient (FissionCLI errs cfg) where
  sendRequest req =
    CLI.withLoader 50_000 do
      manager <- asks $ getField @"httpManager"
      baseUrl <- asks $ getField @"fissionURL"

      liftIO . runClientM req $ mkClientEnv manager baseUrl

instance MonadTime (FissionCLI errs cfg) where
  currentTime = liftIO getCurrentTime

instance MonadRandom (FissionCLI errs cfg) where
  getRandomBytes = liftIO . getRandomBytes

instance ServerDID (FissionCLI errs Connected.Config) where
  getServerDID = do
    did <- asks Connected.serverDID
    logDebug $ "Loaded Server DID: " <> textDisplay did
    return did

instance
  ( DNS.DNSError        `IsMember` errs
  , NotFound DID        `IsMember` errs
  , NotFound FilePath   `IsMember` errs
  , YAML.ParseException `IsMember` errs
  )
  => ServerDID (FissionCLI errs Base.Config) where
  getServerDID = do
    did <- asks Base.serverDID
    logDebug $ "Loaded Server DID: " <> textDisplay did
    return did

instance
  ( IsMember Key.Error errs
  , IsMember (NotFound Ed25519.SecretKey) errs
  , Display (OpenUnion errs)
  , ServerDID (FissionCLI errs cfg)
  , HasField' "fissionURL" cfg BaseUrl
  , HasLogFunc             cfg
  )
  =>  MonadWebAuth (FissionCLI errs cfg) Token where
  getAuth = do
    now       <- currentTime
    sk        <- getAuth
    serverDID <- getServerDID

    let
      jwt =
        JWT.ucan now serverDID sk RootCredential

      rawContent =
        jwt
          |> encode
          |> Lazy.toStrict
          |> decodeUtf8Lenient
          |> Text.dropPrefix "\""
          |> Text.dropSuffix "\""
          |> JWT.contentOf

    return $ Bearer Bearer.Token {..}

instance
  ( Key.Error                  `IsMember` errs
  , NotFound Ed25519.SecretKey `IsMember` errs
  , Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  => MonadWebAuth (FissionCLI errs cfg) Ed25519.SecretKey where -- FIXME switch to MonadKeyStore => MonadWebAuth
  getAuth = do
    attempt (getAsBytes signKey) >>= \case
      Right raw -> ensureM $ Key.Store.parse signKey raw
      Left  _   -> raise $ NotFound @Ed25519.SecretKey
    where
      signKey = Proxy @SigningKey

instance MonadMask (FissionCLI errs cfg) where
  mask action = do
    cfg <- ask
    FissionCLI . RescueT . liftIO $ mask \u ->
      fissionToIO cfg (action $ q cfg u)

  uninterruptibleMask action = do
    cfg <- ask
    FissionCLI . RescueT . liftIO $ uninterruptibleMask \u ->
      fissionToIO cfg (action $ q cfg u)

  generalBracket acquire release use = do
    cfg <- ask

    FissionCLI . RescueT $ liftIO do
      (rb, rme) <- generalBracket
        (fissionToIO cfg acquire)
        (innerRelease cfg)
        (innerUse     cfg)

      return do
        me <- rme
        b  <- rb
        return (b, me)

    where
      innerRelease _ (Left err) _ =
        return $ Left err

      innerRelease cfg (Right resource) exitCase =
        let
          runRelease = fissionToIO cfg . release resource
        in
          case exitCase of
            ExitCaseSuccess (Right val) -> runRelease $ ExitCaseSuccess val
            ExitCaseException err       -> runRelease $ ExitCaseException err
            _                           -> runRelease ExitCaseAbort

      innerUse  _   (Left  err) = return $ Left err
      innerUse cfg (Right val)  = fissionToIO cfg $ use val

fissionToIO :: cfg -> FissionCLI errs cfg a -> IO (Either (OpenUnion errs) a)
fissionToIO cfg action = runRIO cfg . runRescueT $ unFissionCLI action

q :: cfg
  -> (IO (Either (OpenUnion errs) a) -> IO (Either (OpenUnion errs) a))
  -> FissionCLI errs cfg a
  -> FissionCLI errs cfg a
q cfg u = FissionCLI . RescueT . liftIO . u . fissionToIO cfg

instance
  ( Contains errs errs
  , Display (OpenUnion errs)
  , IsMember SomeException errs
  , HasLogFunc cfg
  )
  => MonadCleanup (FissionCLI errs cfg) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire

      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- Catch.uninterruptibleMask_ $
                 fmap (\_ -> ()) (onErr resource errs)
                   `Catch.catch` \(_ :: SomeException) -> return ()

          raise errs

        Right output -> do
          _ <- onOk resource
          return output

instance HasField' "httpManager" cfg HTTP.Manager => MonadManagedHTTP (FissionCLI errs cfg) where
  getHTTPManager = asks $ getField @"httpManager"

instance
  ( HasField' "ipfsTimeout"   cfg IPFS.Timeout
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))
  , HasLogFunc                cfg
  , MonadIPFSIgnore (FissionCLI errs cfg)
  , SomeException `IsMember` errs
  , Contains errs errs
  )
  => MonadLocalIPFS (FissionCLI errs cfg) where
  runLocal opts' arg = do
    logDebug @Text "Running local IPFS"

    ipfsRepo          <- globalIPFSRepo
    IPFS.BinPath ipfs <- globalIPFSBin
    IPFS.Timeout secs <- asks $ getField @"ipfsTimeout"

    pwd        <- getCurrentDirectory
    ignorePath <- IPFS.Ignore.writeTmp . show . Crypto.hash @ByteString @SHA256 $ fromString pwd

    void IPFS.Daemon.runDaemon

    let
      cidVersion = "--cid-version=1"
      timeout    = "--timeout=" <> show secs <> "s"
      ignore     = "--ignore-rules-path=" <> ignorePath
      cmd        = headMaybe opts'
      arg'       = Text.unpack . decodeUtf8Lenient $ Lazy.toStrict arg

      opts =
        if | cmd == Just "swarm"                    -> opts' <> [arg']
           | cmd == Just "pin" || cmd == Just "add" -> opts' <> [arg', timeout, cidVersion, ignore]
           | otherwise                              -> opts' <> [arg', timeout]

      process = intercalate " " ("IPFS_PATH=" <> ipfsRepo : ipfs : opts)

    logDebug $ "Running: " <> process

    Turtle.export "IPFS_PATH" $ Text.pack ipfsRepo

    readProcess (fromString process) >>= \case
      (ExitSuccess, contents, _) ->
        return $ Right contents

      (ExitFailure _, _, stdErrs)
        | Lazy.isSuffixOf "context deadline exceeded" stdErrs ->
            return . Left $ Process.Timeout secs

        | otherwise ->
            return . Left $ Process.UnknownErr stdErrs

instance
  ( HasLogFunc cfg
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))
  )
  => MonadIPFSDaemon (FissionCLI errs cfg) where
  runDaemon = do
    logDebug @Text "Starting IPFS daemon"

    daemonVar <- asks $ getField @"ipfsDaemonVar"

    liftIO (tryReadMVar daemonVar) >>= \case
      Just daemonProcess -> do
        logDebug @Text "IPFS Daemon already running"
        return daemonProcess

      Nothing -> do
        process <- startup

        liftIO (tryPutMVar daemonVar process) >>= \case
          True  -> logDebug @Text "Placed IPFS daemon in MVar"
          False -> logDebug @Text "IPFS Daemon var full"

        return process

    where
      startup ::
        ( MonadIO          m
        , MonadLogger      m
        , MonadEnvironment m
        , MonadIPFSDaemon  m
        )
        => m (Process () () ())
      startup = do
        logDebug @Text "Starting new IPFS Daemon"

        ipfsRepo         <- globalIPFSRepo
        BinPath ipfsPath <- globalIPFSBin

        Turtle.export "IPFS_PATH" $ Text.pack ipfsRepo

        process <- startProcess . fromString $ intercalate " "
          [ "IPFS_PATH=" <> ipfsRepo
          , ipfsPath
          , "daemon"
          , "--enable-pubsub-experiment"
          , "--enable-namesys-pubsub"
          , " > /dev/null 2>&1"
          ]

        logDebug @Text "IPFS daemon started"

        waitForStartup >>= \case
          True  ->
            return process

          False -> do
            logDebug @Text "IPFS daemon startup appears stuck. Retrying."

            stopProcess process
            void IPFS.Daemon.forceStop -- Clean up any existing, on the off chance

            let lockPath = Turtle.decodeString $ ipfsRepo <> "/repo.lock"
            void $ Turtle.touch lockPath
            void $ Turtle.rm    lockPath

            runDaemon

  checkRunning = do
    logDebug @Text "Checking if IPFS daemon is running"

    ipfsRepo         <- globalIPFSRepo
    BinPath ipfsPath <- globalIPFSBin

    let
      command =
        fromString $ intercalate " "
          [ "IPFS_PATH=" <> ipfsRepo
          , ipfsPath
          , "swarm"
          , "addrs"
          , "> /dev/null 2>&1"
          ]

    Turtle.export "IPFS_PATH" $ Text.pack ipfsRepo
    status <- liftIO $ withProcessWait command waitExitCode

    logDebug $ show status
    return $ status == ExitSuccess

waitForStartup :: (MonadIO m, MonadIPFSDaemon m) => m Bool
waitForStartup = go (10 :: Natural)
  where
    go 0 =
      return False

    go count =
      IPFS.Daemon.checkRunning >>= \case
        True  ->
          return True

        False -> do
          threadDelay 1_000_000
          go $ count - 1

instance forall cfg errs .
  ( HasField' "httpManager"   cfg HTTP.Manager
  , HasField' "ipfsURL"       cfg IPFS.URL
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))
  , HasLogFunc                cfg
  , HasProcessContext         cfg
  , SomeException `IsMember` errs
  , Exception (OpenUnion errs)
  , Display   (OpenUnion errs)
  , Contains errs errs
  )
  => IPFS.MonadRemoteIPFS (FissionCLI errs cfg) where
  runRemote query = do
    _ <- IPFS.Daemon.runDaemon
    runBootstrapT $ runRemote query

instance MonadEnvironment (FissionCLI errs cfg) where
  getGlobalPath = do
    home <- getHomeDirectory
    return $ home </> ".config" </> "fission"

instance MonadIPFSIgnore (FissionCLI errs Connected.Config) where
  getIgnoredFiles = asks $ getField @"ignoredFiles"

instance MonadIPFSIgnore (FissionCLI errs Base.Config) where
  getIgnoredFiles = pure []
