-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Connected
  ( runConnected
  , runConnected'
  , liftConfig
  , module Fission.CLI.Config.Connected.Types
  ) where

import           Network.IPFS

import           Fission.Prelude

import Fission.Key.Store as Key

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected.Types
import           Fission.CLI.Config.Connected.Error.Types

import qualified Fission.CLI.Display.Error as CLI.Error

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment
import qualified Fission.CLI.IPFS.Connect      as Connect
 
import           Fission.Web.Client.Peers as Peers

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Display.Loader  as CLI

import qualified Crypto.PubKey.Ed25519    as Ed25519

import Fission.User.DID.Types
import qualified Fission.Key as Key

import qualified Fission.Internal.Base64          as B64

import Servant.Client

import Fission.Web.Auth.Token.JWT
import Fission.Web.Auth.Token

import qualified Fission.Web.Auth.Token.Bearer as Token
import qualified Crypto.PubKey.Ed25519 as Ed25519

import Servant.Client.Core.Auth
import Fission.Web.Client.JWT

import Fission.Authorization.ServerDID


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
  )
  => BaseConfig
  -> m (Either Error ConnectedConfig)
liftConfig BaseConfig {..} = 
  Key.readEd >>= \case
    Left _err -> do -- FIXME add better feedback / there's different errors here!
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
              let
                ignoredFiles = Environment.ignored config
                ucanLink = Nothing

                -- FIXME actually grab the DID
                serverDID = DID
                  { publicKey = Key.Public "AAAAC3NzaC1lZDI1NTE5AAAAIB7/gFUQ9llI1BTrEjW7Jq6fX6JLsK1J4wXK/dn9JMcO"
                  , algorithm = Key.Ed25519
                  , method    = Key
                  }

                cliDID = DID
                  { publicKey = Key.Public . B64.toByteString $ Ed25519.toPublic secretKey
                  , algorithm = Key.Ed25519
                  , method    = Key
                  }

                connCfg = ConnectedConfig {..}

              runConnected' connCfg do
                auth    <- getAuth -- FIXME JWT I guess
                authReq <- mkAuthReq
                let auth' = mkAuthenticatedRequest auth \_ath -> authReq
                sendRequest ((client User.verify) auth') >>= \case -- FIXME make the auth a bearer token directly
                  Left err -> do
                    CLI.Error.notConnected err
                    return $ Left NotRegistered

                  Right _ ->
                    return $ Right connCfg
