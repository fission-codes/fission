-- {-# LANGUAGE UndecidableInstances #-}

-- | General configuration required to run any CLI function
module Fission.CLI.Config.Base.Types
  ( BaseConfig  (..)
  , FissionBase (..)
  ) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Data.ByteString.Char8 as BS8

import           Network.HTTP.Client as HTTP

import           Network.IPFS
import qualified Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.CLI.Display.Loader as CLI

import qualified Fission.Key.Store as Key

import Fission.Authorization.ServerDID
import qualified Network.DNS as DNS
import qualified Fission.Web.Client.JWT as JWT

import           Fission.Key as Key
import           Fission.User.DID.Method.Types as Method


import qualified Crypto.PubKey.Ed25519 as Ed25519
import Servant.Client
import           Control.Monad.Catch

import qualified RIO.ByteString.Lazy as Lazy

import Servant.Client.Core.RunClient

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.Client.Types as Client

import           Fission.CLI.Environment.Class
import           Fission.User.DID.Types


import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import Fission.Web.Auth.Token.JWT
import Fission.Web.Auth.Token
import Fission.Authorization.ServerDID

import Network.HTTP.Client as HTTP
import Fission.Web.Auth.Token.JWT

import qualified Crypto.PubKey.Ed25519    as Ed25519
import Servant.Client.Core.BaseUrl

-- | The configuration used for the CLI application
data BaseConfig = BaseConfig
  { httpManager     :: !HTTP.Manager
  , fissionURL      :: !BaseUrl
  , cachedServerDID :: !(Maybe DID) -- ^ Typically from setting with envar
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
          Left err ->
                -- FIXME make pretty message
            throwM err

          Right [] ->
            error $ "No TXT record at _did." <> BS8.unpack url -- FIXME make pretty message

          Right (didTxt : _) ->
            case eitherDecodeStrict ("\"" <> didTxt <> "\"") of
              Left  errMsg ->
                -- FIXME make pretty
                error $ "Unable to find Fission's ID online: " <> errMsg

              Right did ->
                return did

instance MonadWebAuth FissionBase Token where
  getAuth = do
    now       <- currentTime
    sk        <- getAuth
    serverDID <- getServerDID

    return . Bearer $ Bearer.Token
      { jwt        = JWT.ucan now serverDID sk RootCredential
      , rawContent = Nothing
      }

instance MonadWebAuth FissionBase Ed25519.SecretKey where
  getAuth =
    Key.create >>= \case
      Left Key.AlreadyExists ->
        Key.readEd >>= \case
          Left  err -> error "Unable to create key" -- FIXME make nicely formatted message
          Right key -> return key

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

