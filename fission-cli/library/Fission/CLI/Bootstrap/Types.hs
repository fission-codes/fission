-- | Overrides for when still bootstrapping the system
module Fission.CLI.Bootstrap.Types (BootstrapT (..)) where

import           Network.IPFS                           as IPFS
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client.HTTP.Class

import           Fission.Internal.Orphanage.BaseUrl     ()
import           Fission.Internal.Orphanage.ClientError ()

newtype BootstrapT cfg m a = BootstrapT { runBootstrapT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadManagedHTTP m => MonadManagedHTTP (BootstrapT cfg m) where
  getHTTPManager = BootstrapT getHTTPManager

-- FIXME marked for deletion
instance
  ( MonadIO          m
  , MonadLogger      m
  , MonadManagedHTTP m
  )
  => MonadRemoteIPFS (BootstrapT cfg m) where
  runRemote query = do
    manager <- getHTTPManager
    let url = BaseUrl Https "ipfs.io" 443  ""

    logDebug $ "🌌📞 Making remote IPFS request to HTTP gateway " <> showBaseUrl url

    liftIO (runClientM query $ mkClientEnv manager url) >>= \case
      Left err -> do
        logError $ "🌌💔 Failed to read from remote gateway: " <> textDisplay err
        return $ Left err

      Right val -> do
        logDebug @Text "🌌👍 Remote IPFS success"
        return $ Right val
