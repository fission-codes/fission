-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Connected
  ( run
  , mkConnected
  , module Fission.CLI.Connected.Types
  ) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Data.Yaml                                 as YAML
import qualified RIO.NonEmpty                              as NonEmpty

import qualified Network.HTTP.Client                       as HTTP
import qualified Network.IPFS.Add.Error                    as IPFS.Add
import           Network.IPFS.Local.Class
import qualified Network.IPFS.Process.Error                as IPFS.Process
import qualified Network.IPFS.Timeout.Types                as IPFS
import qualified Network.IPFS.Types                        as IPFS

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types
import qualified Fission.IPFS.Error.Types                  as IPFS
import qualified Fission.JSON                              as JSON
import qualified Fission.Key                               as Key
import           Fission.User.DID.Types                    as DID

import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as JWT.Resolver
import           Fission.Web.Auth.Token.JWT.Types

import           Fission.Web.Client                        as Client

import           Fission.CLI.Connected.Types
import qualified Fission.CLI.Context                       as Context
import           Fission.CLI.Error.Types
import           Fission.CLI.Remote
import           Fission.CLI.Types

import           Fission.CLI.Key.Store                     as Key.Store

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Environment                   as Environment
import           Fission.CLI.Environment.Types             as Environment
import qualified Fission.CLI.IPFS.Connect                  as Connect

import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

type BaseErrs =
  '[ YAML.ParseException
   , JSON.Error
   , NoKeyFile
   , Key.Error
   , IPFS.Process.Error
   , IPFS.Add.Error
   , JWT.Resolver.Error
   , NotFound JWT
   , NotFound FilePath
   , ClientError
   , SomeException
   , IPFS.UnableToConnect
   , NotRegistered
   , NotFound [IPFS.Peer]
   , NotFound Ed25519.SecretKey
   ]

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @Connected@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
run ::
  ( MonadIO m
  , MonadLocalIPFS (FissionCLI errs Config)
  , ServerDID      (FissionCLI errs inCfg)

  , Contains errs     errs
  , Contains BaseErrs errs

  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)

  , HasLogFunc                inCfg
  , HasProcessContext         inCfg
  , HasField' "httpManager"   inCfg HTTP.Manager
  , HasField' "remote"        inCfg Remote
  , HasField' "ipfsDaemonVar" inCfg (MVar (Process () () ()))
  )
  => inCfg
  -> IPFS.Timeout
  -> FissionCLI errs Config a
  -> m (Either (OpenUnion errs) a)
run cfg ipfsTimeout actions =
  runFissionCLI cfg do
    cfg'@Config {peers} <- mkConnected cfg ipfsTimeout

    Context.run cfg' do
      attempt (Connect.swarmConnectWithRetry peers 5) >>= \case
        Left err -> do
          logDebug $ textDisplay err
          Connect.couldNotSwarmConnect
          raise err

        Right () -> do
          logDebug @Text "Connected to remote node"
          actions

mkConnected ::
  ( ServerDID   (FissionCLI errs inCfg)
  , MonadRemote (FissionCLI errs inCfg)

  , Contains errs     errs
  , Contains BaseErrs errs

  , Exception (OpenUnion errs)
  , Display   (OpenUnion errs)

  , HasLogFunc                inCfg
  , HasProcessContext         inCfg

  , HasField' "httpManager"   inCfg HTTP.Manager
  , HasField' "ipfsDaemonVar" inCfg (MVar (Process () () ()))
  )
  => inCfg
  -> IPFS.Timeout -- ^ IPFS timeout in seconds
  -> FissionCLI errs inCfg Config
mkConnected inCfg ipfsTimeout = do
  attempt (Key.Store.fetch $ Proxy @SigningKey) >>= \case
    Left _err -> do
      CLI.Error.put NoKeyFile "Cannot find key. Try 'fission setup' if this is your first time or 'fission user login' if you have an existing account."
      raise NoKeyFile

    Right secretKey -> do
      logDebug @Text "Ed25519 key loaded"

      serverDID  <- getServerDID
      remote     <- getRemote

      config     <- Environment.get
      maybePeers <- Environment.getOrRetrievePeers config

      case NonEmpty.nonEmpty maybePeers of
        Nothing -> do
          logDebug @Text "Cannot load peers"
          CLI.Error.put (NotFound @[IPFS.Peer]) "No peers available"
          raise $ NotFound @[IPFS.Peer]

        Just peers -> do
          logDebug @Text "Loaded peers"

          logFunc       <- asks $ view logFuncL
          processCtx    <- asks $ view processContextL
          ipfsDaemonVar <- asks $ getField @"ipfsDaemonVar"

          let
            ignoredFiles = Environment.ignored config

            cliDID = DID.Key . Key.Ed25519PublicKey $ Ed25519.toPublic secretKey

            cfg = Config { httpManager = getField @"httpManager" inCfg, ..}

          Context.run cfg do
            logDebug @Text "Attempting user verification"
            proof <- getRootUserProof
            attempt (sendAuthedRequest proof whoAmI) >>= \case
              Left err -> do
                CLI.Error.put err "Not registered. Please run: fission user login"
                raise NotRegistered

              Right username -> do
                logDebug $ "User is registered as " <> display username
                return cfg
