module Fission.CLI.Config.Connected.Types
  ( ConnectedConfig  (..)
  , FissionConnected (..)
  ) where

import           Control.Monad.Catch
 
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Network.HTTP.Client as HTTP

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import           Servant.Client

import           Fission.Prelude
import           Fission.Authorization.ServerDID
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT as JWT

import           Fission.CLI.Environment.Class
import           Fission.App.URL.Class

import           Fission.URL.Types as App

data ConnectedConfig = ConnectedConfig
  { httpManager  :: !HTTP.Manager
  , secretKey    :: !Ed25519.SecretKey
  , cliDID       :: !DID
  , serverDID    :: !DID
  -- TODO link systems, ucanLink     :: !JWT
  , appURL       :: !App.URL
  , fissionURL   :: !BaseUrl
  , logFunc      :: !LogFunc
  , processCtx   :: !ProcessContext
  , ipfsPath     :: !IPFS.BinPath
  , ipfsTimeout  :: !IPFS.Timeout
  , peer         :: !IPFS.Peer
  , ignoredFiles :: !IPFS.Ignored
  }

instance HasProcessContext ConnectedConfig where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc ConnectedConfig where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

-- | The top-level app type
newtype FissionConnected a = FissionConnected
  { unwrapFissionConnected :: RIO ConnectedConfig a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader ConnectedConfig
                   , MonadThrow
                   , MonadCatch
                   )

instance MonadLogger FissionConnected where
  monadLoggerLog loc src lvl msg =
    FissionConnected (monadLoggerLog loc src lvl msg)

instance MonadLocalIPFS FissionConnected where
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

instance MonadEnvironment FissionConnected where
  getIgnoredFiles = asks ignoredFiles

instance MonadWebClient FissionConnected where
  sendRequest req = do
    manager <- asks httpManager
    baseUrl <- asks fissionURL

    liftIO . runClientM req $ mkClientEnv manager baseUrl

instance MonadWebAuth FissionConnected DID where
  getAuth = asks cliDID

-- TODO
-- i.e. A UCAN proof
-- instance MonadWebAuth FissionConnected JWT where
--   getAuth = asks ucanLink

instance MonadTime FissionConnected where
  currentTime = liftIO getCurrentTime

instance MonadWebAuth FissionConnected Token where
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

instance MonadWebAuth FissionConnected Ed25519.SecretKey where
  getAuth = asks secretKey

instance ServerDID FissionConnected where
  getServerDID = asks serverDID

instance HasAppURL FissionConnected where
  getAppURL = asks appURL
