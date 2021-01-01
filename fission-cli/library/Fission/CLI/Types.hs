{-# LANGUAGE UndecidableInstances #-}

module Fission.CLI.Types
  ( FissionCLI (..)
  , runFissionCLI
  ) where

import qualified RIO.NonEmpty                               as NonEmpty

import qualified Fission.Web.Auth.Token.JWT                 as UCAN

import qualified Fission.DNS                                as DNS
import           Fission.User.Username.Types


import qualified Data.Bits                                  as Bits
import qualified Data.ByteString.Char8                      as BS8

import qualified RIO.ByteString                             as BS

import           Crypto.Cipher.AES                          (AES256)
import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                     as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                    as RSA
import           Crypto.Random.Types
import           Data.ByteArray                             hiding (all, and,
                                                             length, or)
import qualified Fission.Key.Symmetric.AES256.Payload.Types as AES
import qualified Fission.PubSub.DM.Channel.Types            as DM
import           Fission.PubSub.Secure.Class
import           Fission.Security.EncryptedWith.Types
-- import qualified OpenSSL.RSA                                as OpenSSL

import qualified Fission.Key.Symmetric                      as Symmetric
import           Fission.PubSub.Class
import           Fission.Web.Auth.Token.JWT.Resolver.Class  as JWT
import qualified Network.WebSockets                         as WS
import qualified Network.WebSockets.Client                  as WS
import qualified Wuss                                       as WSS

import           Crypto.Hash                                as Crypto
import qualified Crypto.PubKey.Ed25519                      as Ed25519
-- import           Crypto.Random


import           Fission.CLI.File



import qualified Data.Yaml                                  as YAML
import qualified Fission.CLI.YAML                           as YAML

import           Control.Monad.Base
import           Control.Monad.Catch                        as Catch

import qualified RIO.ByteString.Lazy                        as Lazy
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List                                   as List
import qualified RIO.Map                                    as Map
import qualified RIO.Text                                   as Text

import qualified Network.DNS                                as DNS
import           Network.HTTP.Client                        as HTTP hiding
                                                                     (Proxy)
import           Network.IPFS                               as IPFS
import qualified Network.IPFS.Process.Error                 as Process
import           Network.IPFS.Types                         as IPFS

import           Servant.Client

import qualified Turtle

import           Fission.Prelude                            hiding (mask,
                                                             uninterruptibleMask)

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import qualified Fission.Key.Error                          as Key
import           Fission.User.DID.Types
import           Fission.Web.Client.HTTP.Class

import qualified Fission.CLI.Base.Types                     as Base
import           Fission.CLI.Bootstrap
import qualified Fission.CLI.Connected.Types                as Connected

import           Fission.CLI.IPFS.Daemon                    as IPFS.Daemon
import           Fission.CLI.IPFS.Ignore                    as IPFS.Ignore

import           Fission.CLI.Key.Store                      as Key.Store

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.Bearer.Types        as Bearer
import           Fission.Web.Auth.Token.JWT                 as JWT

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT                     as JWT

import qualified Fission.CLI.Display.Loader                 as CLI
import           Fission.CLI.Environment                    as Env
import           Fission.CLI.Environment.Path

import           Fission.Web.Auth.Token.JWT.Resolver.Error

import qualified Fission.WNFS.Access.Mutation.Store.Class   as WNFS.Mutation
import qualified Fission.WNFS.Access.Query.Store.Class      as WNFS.Query

import           Fission.User.DID.NameService.Class

import           Fission.Internal.Orphanage.BaseUrl         ()
import           Fission.Internal.Orphanage.ClientError     ()
import           Fission.Internal.Orphanage.DNS.DNSError    ()
import           Fission.Internal.Orphanage.OpenUnion       ()

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

instance MonadBase IO (FissionCLI errs cfg) where
  liftBase = liftIO

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

instance forall errs cfg .
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
  ( YAML.ParseException `IsMember` errs
  , NotFound FilePath   `IsMember` errs
  , String              `IsMember` errs
  , Key.Store.Error     `IsMember` errs
  , Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  => WNFS.Mutation.Store (FissionCLI errs cfg) where
  getRootKey targetUsername = do
    Env {..} <- Env.get
    sk       <- Key.Store.fetch $ Proxy @SigningKey

    if username == targetUsername
      then return $ Right sk
      else return $ Left NotFound

  getByCID (CID rawCID) = do
    ucanDir <- globalUCANDir

    let ucanPath = ucanDir </> Text.unpack (rawCID <> ".ucan.jwt")

    attempt (YAML.readFile ucanPath) >>= \case
      Left  _ ->
        return $ Left NotFound

      Right raw ->
        case eitherDecode raw of
          Left  _    -> return $ Left NotFound
          Right ucan -> return $ Right (RawContent (decodeUtf8Lenient $ Lazy.toStrict raw), ucan)

  getCIDsFor (Username nameTxt) path = do
    wnfsDir <- globalWNFSDir

    let
      wnfsUserDir   = wnfsDir     </> Text.unpack nameTxt
      ucanIndexPath = wnfsUserDir </> "ucan_map.yaml"

    attempt (YAML.readFile ucanIndexPath) >>= \case
      Left  _         -> return mempty
      Right ucanIndex -> return (Map.filterWithKey matcher ucanIndex)

    where
      matcher keyPath _ =
        -- LOL sure let's make it a LISP, why not? :P
        or [ keyPath == "*"
           , pathSegments `List.isPrefixOf` splitPath keyPath
           , and [ length path == length keyPath
                 , bitwiseSuperset (BS8.pack keyPath)
                 ]
           ]

      bitwiseSuperset :: ByteString -> Bool
      bitwiseSuperset pathBS2 =
        all (== True) $ BS.zipWith (\x y -> x Bits..&. y == x) pathBS pathBS2

      pathBS :: ByteString
      pathBS = BS8.pack path

      pathSegments :: [FilePath]
      pathSegments = splitPath path

  insert (Username nameTxt) (RawContent rawUCAN) sig = do
    logDebug $ "Writing to UCAN store for " <> nameTxt

    ucanDir <- globalUCANDir
    wnfsDir <- globalWNFSDir

    UCAN.JWT {claims = UCAN.Claims {resource}} <- ensure . eitherDecodeStrict $ encodeUtf8 rawUCAN

    let
      ucanFilename = show (Crypto.hash (encodeUtf8 rawUCAN) :: Digest SHA3_256) <> ".ucan.jwt"
      ucanFilePath = ucanDir </> (ucanFilename <> ".ucan.jwt")

      wnfsUserDir   = wnfsDir     </> Text.unpack nameTxt
      ucanIndexPath = wnfsUserDir </> "ucan_map.yaml"

      newEntry = Map.singleton resource ucanFilePath

    newIndex <- attempt (YAML.readFile ucanIndexPath) >>= \case
                  Left  _        -> return newEntry
                  Right oldIndex -> return $ Map.union newEntry oldIndex

    ucanFilePath  `forceWrite`     (encodeUtf8 rawUCAN <> "." <> convert sig)
    ucanIndexPath `YAML.writeFile` newIndex

instance
  ( YAML.ParseException `IsMember` errs
  , NotFound FilePath   `IsMember` errs
  , String              `IsMember` errs
  , Display (OpenUnion errs)
  , HasLogFunc cfg
  )
  =>  WNFS.Query.Store (FissionCLI errs cfg) where
  insert (Username nameTxt) path key = do
    logDebug $ "Writing to WNFS read key store for " <> nameTxt
    wnfsDir <- globalWNFSDir

    let
      readIndexPath = wnfsDir </> Text.unpack nameTxt </> "read_map.yaml"
      newEntry      = Map.singleton path key

    newIndex <- attempt (YAML.readFile readIndexPath) >>= \case
                  Left  _        -> return newEntry
                  Right oldIndex -> return $ Map.union newEntry oldIndex

    readIndexPath `YAML.writeFile` newIndex

  getKeysFor (Username nameTxt) = do
    wnfsDir <- globalWNFSDir
    let readIndexPath = wnfsDir </> Text.unpack nameTxt </> "read_map.yaml"
    attempt (YAML.readFile readIndexPath) >>= \case
      Left  _         -> return mempty
      Right readIndex -> return readIndex

instance HasLogFunc cfg => MonadNameService (FissionCLI errs cfg) where
  getByUsername (Username rawUsername) = do
    logDebug $ "Fetching DID for " <> rawUsername

    -- FIXME uuugh this is all uglier than it needs to be
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf

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
      url = "_did." <> encodeUtf8 rawUsername <> ".fissionuser.net" -- FIXME environment
                                           -- FIXME ^^^^^^^ make contextual

      notFound = do
        logDebug $ "Unable to find DID for: " <> rawUsername
        return . Left $ NotFound @DID

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
  ( HasLogFunc cfg
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSub (FissionCLI errs cfg) where
  type Connection (FissionCLI errs cfg) = WS.Connection

  connect BaseUrl {..} (Topic rawTopic) withConn = do
    logDebug $ mconcat
      [ "Websocket connecting at: "
      , show baseUrlHost
      , ":"
      , show port
      , path
      ]

    control \runInBase -> do
      WSS.runSecureClient baseUrlHost port path \conn ->
        runInBase do
          logDebug @Text "Websocket pubsub connected"
          withConn conn

    where
      port = fromIntegral baseUrlPort
      path = baseUrlPath <> "/" <> Text.unpack rawTopic

  sendLBS conn msg = do
    logDebug $ "Sending over pubsub: " <> msg
    liftIO . WS.sendDataMessage conn $ WS.Binary msg

  receiveLBS conn = do
    lbs <- liftIO (WS.receiveDataMessage conn) >>= \case
      WS.Text   lbs _ -> return lbs
      WS.Binary lbs   -> return lbs

    logDebug $ "Received message over websockets: " <> lbs
    return lbs

instance
  ( HasLogFunc cfg
  , String    `IsMember` errs -- FIXME better erro r
  , RSA.Error `IsMember` errs -- FIXME better erro r
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSubSecure (FissionCLI errs cfg) (DM.Channel) where
  type SecurePayload (FissionCLI errs cfg) DM.Channel expected =
    expected `EncryptedWith` RSA.PrivateKey

  genSessionKey partnerPK = do
    yourSK <- Key.Store.generate (Proxy @ExchangeKey)
    return DM.Channel {..}

  toSecurePayload DM.Channel {partnerPK} msg = do
    let clearBS = Lazy.toStrict $ encode msg
    cipherBS <- ensureM $ RSA.OAEP.encrypt oaepParams partnerPK clearBS
    return . EncryptedPayload $ Lazy.fromStrict cipherBS

  fromSecurePayload DM.Channel {yourSK} (EncryptedPayload msg) =
    RSA.OAEP.decryptSafer oaepParams yourSK (Lazy.toStrict msg) >>= \case
      Left err -> do
        logDebug $ "Unable to decrypt message via RSA: " <> msg
        raise err

      Right clearBS ->
        case eitherDecodeStrict clearBS of
          -- FIXME better "can't decode JSON" error
          Left err -> do
            logDebug $ "Unable to decode RSA-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
            raise err

          Right payload ->
            return payload

instance
  ( HasLogFunc cfg
  , String    `IsMember` errs -- FIXME better erro r
  , RSA.Error `IsMember` errs -- FIXME better erro r
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSubSecure (FissionCLI errs cfg) RSA.PrivateKey where
  type SecurePayload (FissionCLI errs cfg) RSA.PrivateKey expected =
    expected `EncryptedWith` RSA.PrivateKey

  genSessionKey () = Key.Store.generate (Proxy @ExchangeKey)

  toSecurePayload sk msg = do
    let
      clearBS = Lazy.toStrict $ encode msg
      pk      = RSA.private_pub sk

    logDebug @Text "RSA encrypting pubusb message"
    cipherBS <- ensureM $ RSA.OAEP.encrypt oaepParams pk clearBS
    logDebug $ "Resulting ciphertext: " <> cipherBS
    return . EncryptedPayload $ Lazy.fromStrict cipherBS

  fromSecurePayload sk (EncryptedPayload msg) = do
    logDebug $ "Attempting to decode RSA pubsub payload: " <> msg

    RSA.OAEP.decryptSafer oaepParams sk (Lazy.toStrict msg) >>= \case
      Left err -> do
        logDebug $ "Unable to decrypt message via RSA: " <> msg
        raise err

      Right clearBS ->
        case eitherDecodeStrict clearBS of
          -- FIXME better "can't decode JSON" error
          Left err -> do
            logDebug $ "Unable to decode RSA-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
            raise err

          Right payload ->
            return payload

oaepParams ::
  ( ByteArray       output
  , ByteArrayAccess seed
  )
  => RSA.OAEP.OAEPParams SHA256 seed output
oaepParams = RSA.OAEP.defaultOAEPParams SHA256

instance
  ( HasLogFunc cfg
  , CryptoError `IsMember` errs
  , String `IsMember` errs -- FIXME better error PLZ!
  , Contains errs errs
  , Display (OpenUnion errs)
  )
  => MonadPubSubSecure (FissionCLI errs cfg) (Symmetric.Key AES256) where
  type SecurePayload (FissionCLI errs cfg) (Symmetric.Key AES256) expected =
    AES.Payload expected

  genSessionKey () = Symmetric.genAES256 -- FIXME make an aes256 module

  fromSecurePayload sessionKey AES.Payload {..} =
    case Symmetric.decrypt sessionKey iv secretMessage of
      Left err -> do
        -- FIXME MOVE THIS PART TO the decrypt function, even it that means wrapping in m
        logDebug $ "Unable to decrypt message via AES256: " <> display secretMessage
        raise err

      Right clearLBS ->
        case eitherDecodeStrict $ "Bearer " <> clearLBS of -- FIXME total hack
          -- FIXME better "can't decode JSON" error
          Left err -> do
            logDebug $ "Unable to decode AES-decrypted message. Raw = " <> clearLBS
            raise err

          Right a ->
            return a

  toSecurePayload sessionKey@Symmetric.Key {rawKey} msg = do
    Symmetric.genIV >>= \case
      Nothing ->
        undefined -- FIXME better error

      Just iv -> do
        logDebug $ "SECURE MSG: " <> show (encode msg)
        secretMessage <- ensure $ Symmetric.encrypt sessionKey iv msg
        logDebug $ "SECRET: " <> show secretMessage
        logDebug $ "SECRETb64: " <> textDisplay secretMessage
        return $ AES.Payload {..}

instance
  ( IsMember Key.Error errs
  , IsMember (NotFound Ed25519.SecretKey) errs
  , Display (OpenUnion errs)
  , ServerDID (FissionCLI errs cfg)
  , HasField' "fissionURL" cfg BaseUrl
  , HasLogFunc             cfg
  )
  => MonadWebAuth (FissionCLI errs cfg) Token where
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
        Left $ CannotResolve cid errMsg

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (JWT.contentOf (decodeUtf8Lenient resolvedBS), jwt)

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
