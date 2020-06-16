-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Connected
  ( runConnected
  , runConnected'
  , liftConfig
  , module Fission.CLI.Config.Connected.Types
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Network.IPFS

import           Fission.Prelude
 
import           Fission.Authorization.ServerDID
import qualified Fission.Key as Key
import           Fission.User.DID.Types

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected.Types
import           Fission.CLI.Config.Connected.Error.Types

import qualified Fission.CLI.Display.Error as CLI.Error

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment
import qualified Fission.CLI.IPFS.Connect      as Connect

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @Connected@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
runConnected :: MonadIO m => BaseConfig -> FissionConnected a -> m (Either Error a)
runConnected cfg actions =
  runBase cfg (liftConfig cfg) >>= \case
    Right cfg' -> do
      result <- runConnected' cfg' actions
      return $ Right result

    Left err ->
      return $ Left err

runConnected' :: MonadIO m => ConnectedConfig -> FissionConnected a -> m a
runConnected' cfg actions = runRIO cfg $ unwrapFissionConnected actions

liftConfig ::
  ( MonadUnliftIO  m
  , MonadLocalIPFS m
  , MonadLogger    m
  , MonadWebClient m
  , ServerDID      m
  )
  => BaseConfig
  -> m (Either Error ConnectedConfig)
liftConfig BaseConfig {..} = do
  serverDID <- getServerDID
  Key.readEd >>= \case
    Left _err -> do
      CLI.Error.notConnected NoKeyFile
      return $ Left NoKeyFile
     
    Right secretKey -> do
      config <- Environment.get
 
      Environment.getOrRetrievePeer config >>= \case
        Nothing -> do
          CLI.Error.notConnected PeersNotFound
          return $ Left PeersNotFound

        Just peer ->
          Connect.swarmConnectWithRetry peer 100 >>= \case
            Left err -> do
              logDebug $ displayShow err
              Connect.couldNotSwarmConnect
              return $ Left CannotConnect

            Right _ ->
              let
                ignoredFiles = Environment.ignored config

                cliDID = DID
                  { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic secretKey
                  , method    = Key
                  }

                connCfg = ConnectedConfig {..}

              in
                runConnected' connCfg do
                  logDebug @Text "Connected and attempting user verififcation"
                  sendRequestM (authClient $ Proxy @User.Verify) >>= \case
                    Left err -> do
                      CLI.Error.notConnected err
                      return $ Left NotRegistered

                    Right _ ->
                      return $ Right connCfg
