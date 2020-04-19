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
      response <- CLI.withLoader 50000 $ -- FIXME put loader INSIDE the instance
        undefined -- sendRequest . withAuth ucanJWT $ toEndpoint' User.verify

      case response of
        Left err -> do
          -- CLI.Error.notConnected err -- FIXME bring back
          return $ Left NotRegistered
         
        Right _ -> do
          config <- Environment.get
          Environment.getOrRetrievePeer config >>= \case
            Nothing -> do
              logErrorN "Could not locate the Fission IPFS network"
              return $ Left PeersNotFound
             
            Just peer ->
              Connect.swarmConnectWithRetry peer 1 >>= \case
                Right _ ->
                  let
                    ignoredFiles = Environment.ignored config
                    ucanLink     = Nothing
                    did = DID
                      { publicKey = Key.Public $ B64.toByteString $ Ed25519.toPublic secretKey
                      , algorithm = Key.Ed25519
                      , method    = Key
                      }
                  in
                    return $ Right ConnectedConfig {..}

                Left err -> do
                  logError $ displayShow err
                  Connect.couldNotSwarmConnect
                  return $ Left CannotConnect
