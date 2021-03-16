{-# LANGUAGE UndecidableInstances #-}

module Fission.CLI.Types
  ( FissionCLI (..)
  , runFissionCLI
  ) where

import           Crypto.Cipher.AES                                 (AES256)
import           Crypto.Error
import           Crypto.Hash                                       as Crypto
import qualified Crypto.PubKey.Ed25519                             as Ed25519
import qualified Crypto.PubKey.RSA                                 as RSA
import qualified Crypto.PubKey.RSA.OAEP                            as RSA.OAEP
import           Crypto.Random

import           Data.ByteArray                                    as ByteArray
import qualified Data.Yaml                                         as YAML

import           Control.Monad.Base
import           Control.Monad.Catch                               as Catch

import qualified RIO.ByteString.Lazy                               as Lazy
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List                                          as List
import           RIO.Map                                           as Map
import qualified RIO.NonEmpty                                      as NonEmpty
import qualified RIO.Text                                          as Text

import qualified Turtle

import qualified Network.DNS                                       as DNS
import qualified Network.HTTP.Client                               as HTTP

import           Network.IPFS                                      as IPFS
import qualified Network.IPFS.Add                                  as IPFS
import qualified Network.IPFS.Add.Error                            as IPFS.Add
import qualified Network.IPFS.Process.Error                        as Process
import           Network.IPFS.Types                                as IPFS

import qualified Network.WebSockets.Client                         as WS
import           Servant.Client
import qualified Wuss                                              as WSS

import           Fission.Prelude                                   hiding (mask,
                                                                    uninterruptibleMask)

import           Fission.Authorization.ServerDID
import qualified Fission.DNS                                       as DNS

import           Fission.Error.NotFound.Types

import qualified Fission.JSON                                      as JSON
import qualified Fission.Key.Error                                 as Key
import           Fission.User.DID.Types
import           Fission.Web.Client.HTTP.Class

import qualified Fission.CLI.Base.Types                            as Base
import           Fission.CLI.Bootstrap
import qualified Fission.CLI.Connected.Types                       as Connected

import           Fission.CLI.IPFS.Daemon                           as IPFS.Daemon
import           Fission.CLI.IPFS.Ignore                           as IPFS.Ignore

import qualified Fission.CLI.JSON                                  as JSON

import           Fission.CLI.Key.Store                             as Key.Store

import qualified Fission.CLI.WebNative.FileSystem.Auth             as WebNative.FileSystem.Auth
import qualified Fission.CLI.WebNative.Mutation.Auth               as WebNative.Mutation.Auth

import qualified Fission.CLI.Display.Loader                        as CLI
import           Fission.CLI.Environment                           as Env
import           Fission.CLI.Environment.Path

import qualified Fission.Web.Auth.Token.Bearer.Types               as Bearer
import           Fission.Web.Auth.Token.JWT                        as JWT

import qualified Fission.Key.Asymmetric.Public                     as Asymmetric
import           Fission.Key.EncryptedWith.Types
import qualified Fission.Key.IV.Error                              as IV
import qualified Fission.Key.Symmetric                             as Symmetric

import           Fission.User.DID.NameService.Class                as DID
import           Fission.User.Username

import           Fission.Web.Auth.Token.JWT.Resolver               as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver               as JWT.Resolver
import           Fission.Web.Auth.Token.Types

import           Fission.Web.Client

import           Fission.Internal.Orphanage.BaseUrl                ()
import           Fission.Internal.Orphanage.CID                    ()
import           Fission.Internal.Orphanage.DNS.DNSError           ()
import           Fission.Internal.Orphanage.OpenUnion              ()

import           Fission.Internal.Orphanage.ClientError            ()

import           Fission.CLI.PubSub
import           Fission.CLI.Remote

import           Fission.CLI.PubSub.Secure.Class
import qualified Fission.CLI.PubSub.Secure.Payload.AES.Types       as AES
import           Fission.CLI.PubSub.Secure.Payload.Class
import           Fission.CLI.PubSub.Secure.Payload.Error
import qualified Fission.CLI.PubSub.Secure.Session.Handshake.Types as PubSub
import qualified Fission.CLI.PubSub.Secure.Session.Types           as PubSub

import           Fission.Internal.Orphanage.BaseUrl                ()
import           Fission.Internal.Orphanage.ClientError            ()
import           Fission.Internal.Orphanage.DNS.DNSError           ()
import           Fission.Internal.Orphanage.OpenUnion              ()

newtype FissionCLI errs cfg a = FissionCLI
  { unFissionCLI :: RescueT errs (RIO cfg) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader cfg
                   , MonadThrow
                   , MonadCatch
                   , MonadBase IO
                   )

runFissionCLI :: MonadIO m => cfg -> FissionCLI errs cfg a -> m (Either (OpenUnion errs) a)
runFissionCLI cfg = runRIO cfg . runRescueT . unFissionCLI

instance
  ( HasLogFunc cfg
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadBaseControl IO (FissionCLI errs cfg) where
  type StM (FissionCLI errs cfg) a = Either (OpenUnion errs) a

  -- NOTE type RunInBase ~ FissionCLI errs cfg a -> IO (Either errs a)
  liftBaseWith runner = do
    cfg <- ask
    liftIO  $ runner \action -> runFissionCLI cfg action

  restoreM = \case
    Left  err -> raise err
    Right val -> pure val

instance forall errs cfg.
  ( Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  => MonadRaise (FissionCLI errs cfg) where
  type Errors (FissionCLI errs cfg) = errs

  raise err = do
    logWarn $ "🙋 " <> display (include err :: OpenUnion errs)
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
  , HasLogFunc              cfg

  , MonadRemote (FissionCLI errs cfg)
  )
  => MonadWebClient (FissionCLI errs cfg) where
  sendRequest req =
    CLI.withLoader 50_000 do
      manager <- asks $ getField @"httpManager"
      remote  <- getRemote

      liftIO . runClientM req . mkClientEnv manager $ toBaseUrl remote

instance MonadTime (FissionCLI errs cfg) where
  currentTime = liftIO getCurrentTime

instance MonadRandom (FissionCLI errs cfg) where
  getRandomBytes = liftIO . getRandomBytes

instance ServerDID (FissionCLI errs Connected.Config) where
  getServerDID = do
    did <- asks Connected.serverDID
    logDebug $ "⚙️ 🆔 Loaded Server DID: " <> textDisplay did
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
    logDebug $ "⚙️ 🆔 Loaded Server DID: " <> textDisplay did
    return did

instance
  ( JSON.Error        `IsMember` errs
  , NotFound FilePath `IsMember` errs
  , Process.Error     `IsMember` errs
  , SomeException     `IsMember` errs
  , IPFS.Add.Error    `IsMember` errs
  , Contains errs errs
  , HasLogFunc cfg
  , HasField' "ipfsTimeout"   cfg IPFS.Timeout
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))
  , MonadIPFSIgnore (FissionCLI errs cfg)
  , Display (OpenUnion errs)
  )
  => WebNative.Mutation.Auth.MonadStore (FissionCLI errs cfg) where
  insert token@Bearer.Token {jwt = JWT {sig}, rawContent = RawContent ogContent} = do
    logDebug @Text "🛂💾 Adding UCAN to store"
    storePath      <- ucanStorePath
    store          <- WebNative.Mutation.Auth.getAll

    _              <- ensureM $ IPFS.addFile (encode ogUCAN) "ucan.jwt" -- For CID references
    (_, CID hash') <- ensureM $ IPFS.addFile (encode token)  "bearer.jwt"

    let
      -- FIXME fix in ipfs-haskell smart constructor
      cleanHash =
        hash'
          |> Text.dropPrefix "\""
          |> Text.dropSuffix "\""

      cid          = CID cleanHash
      newStoreJSON = Map.insert cid token store

    logDebug $ "🛂🗃️  Writing updated UCAN store: " <> displayShow newStoreJSON
    storePath `JSON.writeFile` newStoreJSON

    return cid
    where
      ogUCAN :: Text
      ogUCAN = ogContent <> "." <> textDisplay sig

  getAll = do
    logDebug @Text "🛂🚚 Loading UCAN store"
    storePath <- ucanStorePath
    attempt (JSON.readFile storePath) >>= \case
      Left  _     -> return mempty
      Right store -> return store

instance
  ( HasLogFunc cfg
  , JSON.Error        `IsMember` errs
  , NotFound FilePath `IsMember` errs
  , Display (OpenUnion errs)
  )
  => WebNative.FileSystem.Auth.MonadStore (FissionCLI errs cfg) where
  set did subGraphRoot aesKey = do
    logDebug $ "Storing AES key for " <> display did <> " @ " <> displayShow subGraphRoot

    storePath  <- wnfsKeyStorePath
    storeOrErr <- attempt $ JSON.readFile storePath

    let
      store = case storeOrErr of
        Left  _                                        -> mempty
        Right (WebNative.FileSystem.Auth.Store store') -> store'

      oldDIDStore    = fromMaybe mempty (store !? did)
      newDIDStore    = Map.insert subGraphRoot aesKey oldDIDStore
      newGlobalStore = Map.insert did newDIDStore store

    logDebug @Text "✍️  Writing updated WNFS store"
    storePath `JSON.writeFile` WebNative.FileSystem.Auth.Store newGlobalStore

  getAllMatching did subGraphRoot = do
    logDebug $ "Looking up AES key for " <> display did <> " @ " <> displayShow subGraphRoot

    storePath  <- wnfsKeyStorePath
    storeOrErr <- attempt $ JSON.readFile storePath

    let
      store = case storeOrErr of
        Left  _                                        -> mempty
        Right (WebNative.FileSystem.Auth.Store store') -> store'

    (store !? did)
      |> fromMaybe mempty
      |> Map.filterWithKey (\path _ -> path `List.isPrefixOf` subGraphRoot)
      |> pure

instance
  ( Key.Error                  `IsMember` errs
  , JSON.Error                 `IsMember` errs
  , YAML.ParseException        `IsMember` errs
  , NotFound FilePath          `IsMember` errs
  , Process.Error              `IsMember` errs
  , SomeException              `IsMember` errs
  , JWT.Resolver.Error         `IsMember` errs
  , NotFound Ed25519.SecretKey `IsMember` errs
  , NotFound JWT               `IsMember` errs
  , IPFS.Add.Error             `IsMember` errs
  , Contains errs errs

  , HasField' "ipfsTimeout"   cfg IPFS.Timeout
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))

  , Display (OpenUnion errs)
  , HasLogFunc cfg

  , MonadIPFSIgnore (FissionCLI errs cfg)
  , ServerDID       (FissionCLI errs cfg)
  )
  => MonadWebAuth (FissionCLI errs cfg) Token where
  getAuth = do
    logDebug @Text "👀🛂 Fetching Token auth"

    now       <- currentTime
    serverDID <- getServerDID
    sk        <- getAuth
    proof     <- do
      attempt Env.get >>= \case
        Left _ -> do
          return RootCredential

        Right Env {rootProof} ->
          case rootProof of
            Nothing -> do
              return RootCredential

            Just cid -> do
              store <- WebNative.Mutation.Auth.getAll
              case store !? cid of
                Nothing                -> raise  $ NotFound @JWT
                Just Bearer.Token {..} -> return $ Nested rawContent jwt

    logDebug $ "🧾🛂 Proof: " <> display proof

    let
      jwt =
        JWT.delegateAppendAll serverDID sk proof now

      rawContent =
        jwt
          |> encode
          |> Lazy.toStrict
          |> decodeUtf8Lenient
          |> Text.dropPrefix "\""
          |> Text.dropSuffix "\""
          |> JWT.contentOf

      token =
        Bearer Bearer.Token {..}

    logDebug $ "🏗️ 🛂 Constructed token: " <> decodeUtf8Lenient (Lazy.toStrict $ encode token)
    return token

instance
  ( Key.Error                  `IsMember` errs
  , NotFound Ed25519.SecretKey `IsMember` errs
  , Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  => MonadWebAuth (FissionCLI errs cfg) Ed25519.SecretKey where
  getAuth = do
    attempt (getAsBytes $ Proxy @SigningKey) >>= \case
      Right raw -> ensureM $ Key.Store.parse (Proxy @SigningKey) raw
      Left  _   -> raise $ NotFound @Ed25519.SecretKey

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

instance HasField' "remote" cfg Remote => MonadRemote (FissionCLI errs cfg) where
  getRemote = asks $ getField @"remote"

instance (HasLogFunc cfg, HasField' "remote" cfg Remote) => MonadNameService (FissionCLI errs cfg) where
  getByUsername username = do
    logDebug $ "Fetching DID for " <> display username

    rs      <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    nameURL <- getNameService

    let url = toDNS username nameURL

    liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
      Left _ ->
        notFound

      Right listBS -> do
        logDebug $ "Got raw DID response: " <> show listBS
        case NonEmpty.nonEmpty (decodeUtf8Lenient <$> listBS) of
          Nothing ->
            notFound

          Just segments -> do
            let rawDID = DNS.mergeSegments segments
            logDebug $ "Raw DID: " <> rawDID
            case decode $ encode rawDID  of
              Nothing  -> notFound
              Just did -> return $ Right did

    where
      notFound = do
        logDebug $ "Unable to find DID for: " <> display username
        return . Left $ NotFound @DID

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
    logDebug @Text "🌌🎬 Running local IPFS"

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

      (pipeArg, opts) =
        if | cmd == Just "swarm"                            -> (Nothing,  opts' <> [arg'])
           | List.elem "--stdin-name" opts'                 -> (Just arg, opts' <> [timeout, cidVersion, ignore])
           | cmd == Just "pin" || cmd == Just "add"         -> (Nothing,  opts' <> [arg', timeout, cidVersion, ignore])
           | otherwise                                      -> (Nothing,  opts' <> [arg', timeout])

      processStr = intercalate " " ("IPFS_PATH=" <> ipfsRepo : ipfs : opts)
      rawProcess = fromString processStr
      process    =
        case pipeArg of
          Nothing    -> rawProcess
          Just inArg -> setStdin (byteStringInput inArg) rawProcess

    logDebug $ "🌌⚙️  Running: " <> processStr <> " with arg " <> show pipeArg
    Turtle.export "IPFS_PATH" $ Text.pack ipfsRepo

    readProcess process >>= \case
      (ExitSuccess, contents, _) -> do
        logDebug @Text "🌌 IPFS process completed successfully"
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
    logDebug @Text "😈🎬 Starting IPFS daemon"

    daemonVar <- asks $ getField @"ipfsDaemonVar"

    liftIO (tryReadMVar daemonVar) >>= \case
      Just daemonProcess -> do
        logDebug @Text "😈⚙️  IPFS Daemon already running"
        return daemonProcess

      Nothing -> do
        process <- startup

        liftIO (tryPutMVar daemonVar process) >>= \case
          True  -> logDebug @Text "😈📦 Placed IPFS daemon in MVar"
          False -> logWarn  @Text "👿🈵 IPFS Daemon var full"

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
        logDebug @Text "😈🎬 Starting new IPFS Daemon"

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

        logDebug @Text "😈🏁 IPFS daemon started"

        waitForStartup >>= \case
          True  ->
            return process

          False -> do
            logWarn @Text "👿🔁 IPFS daemon startup appears stuck. Retrying."

            stopProcess process
            void IPFS.Daemon.forceStop -- Clean up any existing, on the off chance

            let lockPath = Turtle.decodeString $ ipfsRepo <> "/repo.lock"
            void $ Turtle.touch lockPath
            void $ Turtle.rm    lockPath

            runDaemon

  checkRunning = do
    logDebug @Text "😈🔭 Checking if IPFS daemon is running"

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

    logDebug $ "😈🌡️  " <> show status
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
instance
  ( HasLogFunc cfg
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSub (FissionCLI errs cfg) where
  type Connection (FissionCLI errs cfg) = WS.Connection

  connect BaseUrl {..} (Topic rawTopic) withConn = do
    logDebug $ mconcat
      [ "📞🙏 Websocket connecting at: "
      , baseUrlHost
      , ":"
      , show port
      , path
      ]

    control \runInBase -> do
      WSS.runSecureClient baseUrlHost port path \conn ->
        runInBase do
          logDebug @Text "📞🔗 Websocket pubsub connected"
          withConn conn

    where
      port = fromIntegral baseUrlPort
      path = baseUrlPath <> "/" <> Text.unpack rawTopic

  sendLBS conn msg = do
    logDebug @Text "📞🗣️  Sending over pubsub:"
    logDebug msg
    liftIO . WS.sendDataMessage conn $ WS.Binary msg

  receiveLBS conn = do
    logDebug @Text "📞👂 Listening for pubsub-over-websockets message..."

    msg <- liftIO (WS.receiveDataMessage conn)
    lbs <- case msg of
      WS.Text   lbs _ -> return lbs
      WS.Binary lbs   -> return lbs

    logDebug $ "Received message over websockets: " <> lbs
    return lbs

instance
  ( HasLogFunc cfg
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSubSecure (FissionCLI errs cfg) (Symmetric.Key AES256) where
  genSessionKey () = Symmetric.genAES256

instance
  ( HasLogFunc cfg
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSubSecure (FissionCLI errs cfg) (RSA.PublicKey, RSA.PrivateKey) where
  genSessionKey () = do
    sk <- Asymmetric.genRSA2048
    return (RSA.private_pub sk, sk)

instance forall errs cfg .
  ( HasLogFunc cfg
  , IV.GenError `IsMember` errs
  , CryptoError `IsMember` errs
  , RSA.Error   `IsMember` errs
  , Display (OpenUnion errs)
  )
  => MonadSecured (FissionCLI errs cfg) (RSA.PublicKey, RSA.PrivateKey) PubSub.Session where
  toSecurePayload (rsaPK, _) PubSub.Session {bearerToken, sessionKey = sessionKey@(Symmetric.Key aesClear)} = do
    logDebug @Text "Encrypting RSA-secured PubSub.Session payload (Handshake)"

    iv           <- ensureM   Symmetric.genIV
    msg          <- ensure  $ Symmetric.encrypt sessionKey iv bearerToken
    encryptedAES <- ensureM $ RSA.OAEP.encrypt oaepParams rsaPK aesClear

    return PubSub.Handshake { iv
                            , msg
                            , sessionKey = EncryptedPayload $ Lazy.fromStrict encryptedAES
                            }

  fromSecurePayload (_, rsaSK) PubSub.Handshake {iv, sessionKey = EncryptedPayload keyInRSA, msg = tokenInAES} = do
    logDebug @Text "Decrypting RSA-secured PubSub.Session payload (Handshake)"
    RSA.OAEP.decryptSafer oaepParams rsaSK (Lazy.toStrict keyInRSA) >>= \case
      Left rsaError ->
        return . Left $ CannotDecryptRSA rsaError

      Right symmetricKeyActual -> do
        logDebug @Text "Decrypted session key successfully"

        let
          sessionKey = Symmetric.Key symmetricKeyActual

        case Symmetric.decrypt sessionKey iv tokenInAES of
          Left cryptoError ->
            return . Left $ CannotDecrypt cryptoError

          Right bs -> do
            logDebug $ "Decrypted still-serialized brearer token: " <> bs
            case eitherDecode $ encode ("Bearer " <> decodeUtf8Lenient bs) of
              Left err          -> return . Left $ UnableToDeserialize err
              Right bearerToken -> return $ Right PubSub.Session {..}

oaepParams :: (ByteArray output, ByteArrayAccess seed) => RSA.OAEP.OAEPParams SHA256 seed output
oaepParams = RSA.OAEP.defaultOAEPParams SHA256

instance forall errs cfg msg .
  ( HasLogFunc cfg
  , ToJSON   msg
  , FromJSON msg
  , IV.GenError `IsMember` errs
  , CryptoError `IsMember` errs
  , Display (OpenUnion errs)
  )
  => MonadSecured (FissionCLI errs cfg) (Symmetric.Key AES256) msg where
  toSecurePayload aesKey msg = do
    iv            <- ensureM Symmetric.genIV
    secretMessage <- ensure $ Symmetric.encrypt aesKey iv msg
    return AES.Payload {..}

  fromSecurePayload aesKey AES.Payload {..} =
    case Symmetric.decrypt aesKey iv secretMessage of
      Left cryptoError ->
        return . Left $ CannotDecrypt cryptoError

      Right bs ->
        case eitherDecodeStrict bs of
          Left err -> return . Left $ UnableToDeserialize err
          Right a  -> return $ Right a

instance
  ( HasLogFunc                cfg
  , HasField' "ipfsTimeout"   cfg IPFS.Timeout
  , HasField' "ipfsDaemonVar" cfg (MVar (Process () () ()))
  , IsMember SomeException errs
  , Contains errs errs
  , MonadIPFSIgnore (FissionCLI errs cfg)
  )
  => JWT.Resolver (FissionCLI errs cfg) where
  resolve cid@(IPFS.CID hash') =
    IPFS.runLocal ["cat"] (Lazy.fromStrict $ encodeUtf8 hash') <&> \case
      Left errMsg ->
        Left $ CannotResolve cid (ConnectionError $ toException errMsg)

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (JWT.contentOf (decodeUtf8Lenient resolvedBS), jwt)

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
    logDebug @Text "Running remote IPFS"
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
