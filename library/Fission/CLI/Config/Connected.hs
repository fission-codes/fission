-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Connected
  ( runConnected
  , runConnected'
  , liftConfig
  , module Fission.CLI.Config.Connected.Types
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Network.IPFS
import           RIO.Directory

import           Fission.Prelude
 
import           Fission.Authorization.ServerDID
import qualified Fission.Key as Key
import           Fission.User.DID.Types
import           Fission.URL.Types

import qualified Fission.Web.Auth.Token.Types as Auth

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.App  as App
import qualified Fission.Web.Client.User as User

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected.Types
import           Fission.CLI.Config.Connected.Error.Types

import           Fission.CLI.Environment.Override as Override
import qualified Fission.CLI.Display.Error        as CLI.Error

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
  , MonadTime      m
  , MonadWebClient m
  , MonadWebAuth   m Auth.Token
  , MonadWebAuth   m Ed25519.SecretKey
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
          logErrorN "Could not locate the Fission IPFS network"
          return $ Left PeersNotFound

        Just peer ->
          Connect.swarmConnectWithRetry peer 1 >>= \case
            Left err -> do
              logError $ displayShow err
              Connect.couldNotSwarmConnect
              return $ Left CannotConnect

            Right _ -> do
              getOrCreateApp config >>= \case
                Left err ->
                  return $ Left err

                Right appURL -> do
                  let
                    ignoredFiles = Environment.ignored config

                    cliDID = DID
                      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic secretKey
                      , method    = Key
                      }

                    connCfg = ConnectedConfig {..}

                  runConnected' connCfg do
                    sendRequestM (authClient $ Proxy @User.Verify) >>= \case
                      Left err -> do
                        CLI.Error.notConnected err
                        return $ Left NotRegistered

                      Right _ ->
                        return $ Right connCfg

getOrCreateApp ::
  ( MonadWebClient m
  , MonadIO m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Auth.Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Environment
  -> m (Either Error URL)
getOrCreateApp config =
  case (config |> Environment.appURL) of
    Just val ->
      return $ Right val

    Nothing ->
      sendRequestM (authClient $ Proxy @App.Create) >>= \case
        Left _ ->
          return $ Left NoApp

        Right appURL -> do
          path <- liftIO getCurrentDirectory
          Override.writeMerge path $ mempty { maybeAppURL = Just appURL }
          return $ Right appURL
