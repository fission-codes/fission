-- | General configuration required to run any CLI function
module Fission.CLI.Config.Base.Types
  ( BaseConfig  (..)
  , FissionBase (..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Directory
import qualified RIO.Text            as Text

import qualified Data.ByteString.Char8 as BS8

import qualified Network.DNS         as DNS
import qualified Network.HTTP.Client as HTTP

import           Network.IPFS
import qualified Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import           Servant.Client

import           Fission.Prelude

import           Fission.URL
 
import           Fission.App.URL.Class
import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import           Fission.Key as Key
import           Fission.User.DID.Types
import           Fission.CLI.Environment.Override as Override

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT as JWT
import qualified Fission.Web.Client.App as App
 
import qualified Fission.CLI.Display.Error  as CLI.Error
import qualified Fission.CLI.Display.Loader as CLI

-- | The configuration used for the CLI application
data BaseConfig = BaseConfig
  { httpManager     :: !HTTP.Manager
  , fissionURL      :: !BaseUrl
  , cachedServerDID :: !(Maybe DID) -- ^ Typically from setting with envar
  , cachedAppURL    :: !(Maybe URL)
  , logFunc         :: !LogFunc
  , processCtx      :: !ProcessContext
  , ipfsPath        :: !IPFS.BinPath
  , ipfsTimeout     :: !IPFS.Timeout
  }

instance HasProcessContext BaseConfig where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc BaseConfig where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

-- | The top-level app type
newtype FissionBase a = FissionBase { unwrapFissionBase :: RIO BaseConfig a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader BaseConfig
                   , MonadThrow
                   )

instance MonadLogger FissionBase where
  monadLoggerLog loc src lvl msg = FissionBase (monadLoggerLog loc src lvl msg)

instance MonadWebClient FissionBase where
  sendRequest req =
    CLI.withLoader 50_000 do
      manager <- asks httpManager
      baseUrl <- asks fissionURL

      liftIO . runClientM req $ mkClientEnv manager baseUrl
 
instance MonadTime FissionBase where
  currentTime = liftIO getCurrentTime

instance ServerDID FissionBase where
  getServerDID = do
    asks cachedServerDID >>= \case
      Just did ->
        return did
       
      Nothing -> do
        rs      <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
        baseURL <- asks fissionURL
        let url = BS8.pack $ "_did." <> baseUrlHost baseURL

        logDebugN $ "Checking TXT " <> decodeUtf8Lenient url

        liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
          Left err -> do
            CLI.Error.put err "Unable to find Fission's ID online"
            throwM err

          Right [] -> do
            CLI.Error.put (NotFound @DID) $
              "No TXT record at " <> decodeUtf8Lenient url
             
            throwM $ NotFound @DID

          Right (didTxt : _) ->
            case eitherDecodeStrict ("\"" <> didTxt <> "\"") of
              Left  err -> do
                CLI.Error.put err "Unable to find Fission's ID online"
                throwM $ NotFound @DID

              Right did ->
                return did

instance MonadWebAuth FissionBase Token where
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

    return . Bearer $ Bearer.Token {..}

instance MonadWebAuth FissionBase Ed25519.SecretKey where
  getAuth =
    Key.create >>= \case

      Left Key.AlreadyExists ->
        Key.readEd >>= \case
          Left err -> do
            CLI.Error.put err "Unable to find or create key"
            throwM $ NotFound @Ed25519.SecretKey

          Right key ->
            return key

      Left err -> do
        CLI.Error.put err "Unable to create key"
        throwM $ NotFound @Ed25519.SecretKey

      Right _ ->
        getAuth

instance MonadLocalIPFS FissionBase where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks ipfsPath
    IPFS.Timeout secs <- asks ipfsTimeout

    let opts' = ("--timeout=" <> show secs <> "s") : opts

    runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' <&> \case
      (ExitSuccess, contents, _) ->
        Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            Left $ Process.Timeout secs

        | otherwise ->
            Left $ Process.UnknownErr stdErr

instance HasAppURL FissionBase where
  getAppURL =
    asks cachedAppURL >>= \case
      Just appURL ->
        return appURL

      Nothing ->
        sendRequestM (authClient $ Proxy @App.Create) >>= \case
          Left _err ->
            error "unable to connect to server to create app" -- FIXME rescue shoudl fix this straightforwardly
           
          Right appURL -> do
            path <- liftIO getCurrentDirectory
            Override.writeMerge path $ mempty { maybeAppURL = Just appURL }
            return appURL
