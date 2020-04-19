module Fission.CLI.Config.Connected.Types
  ( ConnectedConfig (..)
  , FissionConnected (..)
  ) where

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


import Fission.Web.Auth.Token.JWT
import Fission.Web.Auth.Token
import Fission.Authorization.ServerDID

import Network.HTTP.Client as HTTP
import Fission.Web.Auth.Token.JWT

import qualified Crypto.PubKey.Ed25519    as Ed25519
import Servant.Client.Core.BaseUrl

data ConnectedConfig = ConnectedConfig
  { httpManager  :: !HTTP.Manager
  , secretKey    :: !Ed25519.SecretKey
  , cliDID       :: !DID
  , serverDID    :: !DID
  , ucanLink     :: !(Maybe JWT)
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
newtype FissionConnected a = FissionConnected { unwrapFissionConnected :: RIO ConnectedConfig a } -- FIXME make the getter shorter!
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
  monadLoggerLog loc src lvl msg = FissionConnected (monadLoggerLog loc src lvl msg)

-- instance MonadWebClient FissionConnected where
--   run cmd = do
--     Client.Runner runner <- asks fissionAPI
--     liftIO $ runner cmd

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

-- FIXME paramaterize Fission, maybe?
instance MonadWebClient FissionConnected where
  sendRequest req = do
    manager <- asks httpManager
    baseUrl <- asks fissionURL

    liftIO . runClientM req $ mkClientEnv manager baseUrl

instance MonadWebAuth FissionConnected DID where
  getAuth = asks cliDID

-- i.e. A UCAN proof
instance MonadWebAuth FissionConnected (Maybe JWT) where
  getAuth = asks ucanLink

instance MonadTime FissionConnected where
  currentTime = liftIO getCurrentTime

instance MonadWebAuth FissionConnected Token where
  getAuth = undefined -- FIXME!

instance MonadWebAuth FissionConnected Ed25519.SecretKey where -- Probably actualluy want a MonadSigningKey or something
  getAuth = asks secretKey

instance ServerDID FissionConnected where
  getServerDID = asks serverDID
  -- publicize = putStrLn
