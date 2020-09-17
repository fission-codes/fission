{-# LANGUAGE UndecidableInstances #-}

-- | Overrides for when still bootstrapping the system

module Fission.CLI.Bootstrap.Types (BootstrapT (..)) where

import qualified Network.DNS                            as DNS
import           Network.HTTP.Client                    as HTTP
import           Network.IPFS                           as IPFS
import           Network.IPFS                           as IPFS
import           Network.IPFS.Process
import qualified Network.IPFS.Process.Error             as Process
import           Network.IPFS.Types                     as IPFS

import           Servant.Client

import           Fission.Prelude

import           Fission.Internal.Orphanage.BaseUrl     ()
import           Fission.Internal.Orphanage.ClientError ()

newtype BootstrapT cfg m a = BootstrapT { runBootstrapT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadReader cfg m => MonadReader cfg (BootstrapT cfg m) where
  ask = BootstrapT ask

instance
  ( MonadIO         m
  , MonadLogger     m
  , MonadReader cfg m
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "ipfsURL"     cfg IPFS.URL
  )
  => MonadRemoteIPFS (BootstrapT cfg m) where
  runRemote query = do
    IPFS.URL url <- asks $ getField @"ipfsURL"
    manager      <- asks $ getField @"httpManager"

    logDebug $ "Making remote IPFS request to HTTP gateway " <> textDisplay url

    liftIO (runClientM query $ mkClientEnv manager url) >>= \case
      Left err -> do
        logError $ "Failed to read from remote gateway: " <> textDisplay err
        return $ Left err

      Right val -> do
        logDebug @Text "Remote IPFS success"
        return $ Right val
