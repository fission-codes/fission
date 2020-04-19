-- {-# LANGUAGE UndecidableInstances #-}

-- | General configuration required to run any CLI function
module Fission.CLI.Config.Base.Types
  ( BaseConfig  (..)
  , FissionBase (..)
  ) where

import qualified RIO.ByteString.Lazy as Lazy
-- import           RIO.Orphans () -- FIXME please move this to Prelude!

import Servant.Client.Core.RunClient

import           Servant.Client
 
import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.Client.Types as Client

import           Network.IPFS
import qualified Network.IPFS.Types       as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import Network.HTTP.Client as HTTP

import Servant.Client.Core.BaseUrl

-- | The configuration used for the CLI application
data BaseConfig = BaseConfig
  { httpManager :: !HTTP.Manager
  , fissionURL  :: !BaseUrl
  , logFunc     :: !LogFunc
  , processCtx  :: !ProcessContext
  , ipfsPath    :: !IPFS.BinPath
  , ipfsTimeout :: !IPFS.Timeout
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
  sendRequest req = do
    manager <- asks httpManager
    baseUrl <- asks fissionURL

    liftIO . runClientM req $ mkClientEnv manager baseUrl

-- instance HasClient ClientM api => MonadUnauthedEndpoint api FissionBase where
--   toUnauthedEndpoint pxy _ = do
--     manager <- asks httpManager
--     baseUrl <- asks fissionURL

--     let
--       req = (client pxy)
--       env = mkClientEnv manager baseUrl

--     liftIO $ runClientM req env

-- instance RunClient FissionBase where
--   -- runRequest :: Request -> m Response
--   -- runClientM :: ClientM a -> ClientEnv -> IO (Either ClientError a)
--   runRequest req = do

-- instance HasClient FissionBase api where
--   type Client FissionBase api = FissionBase ()

  -- clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api
  -- clientWithRoute _ pxyAPI req = do
  --   -- runClientM :: ClientM a -> ClientEnv -> IO (Either ClientError a)
  --   runClientM req

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

