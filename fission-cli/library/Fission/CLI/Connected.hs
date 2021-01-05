-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Connected
  ( run
  , mkConnected
  , module Fission.CLI.Connected.Types
  ) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import qualified Data.Yaml                       as YAML
import qualified RIO.NonEmpty                    as NonEmpty

import qualified Network.HTTP.Client             as HTTP
import           Network.IPFS.Local.Class
import qualified Network.IPFS.Timeout.Types      as IPFS
import qualified Network.IPFS.Types              as IPFS
import           Servant.Client

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types
import qualified Fission.IPFS.Error.Types        as IPFS
import qualified Fission.Key                     as Key
import           Fission.User.DID.Types

import           Fission.Web.Client              as Client
import qualified Fission.Web.Client.User         as User

import           Fission.CLI.Connected.Types
import qualified Fission.CLI.Context             as Context
import           Fission.CLI.Error.Types
import           Fission.CLI.Types

import qualified Fission.CLI.Key.Ed25519         as Ed25519
import qualified Fission.CLI.Key.Store           as Key.Store

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Environment         as Environment
import           Fission.CLI.Environment.Types   as Environment
import qualified Fission.CLI.IPFS.Connect        as Connect

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @Connected@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
run ::
  ( MonadIO m
  , MonadLocalIPFS (FissionCLI errs Config)
  , ServerDID      (FissionCLI errs inCfg)

  , Contains errs     errs
  , Contains LiftErrs errs
  , IsMember YAML.ParseException  errs
  , IsMember IPFS.UnableToConnect errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)

  , HasLogFunc                inCfg
  , HasProcessContext         inCfg
  , HasField' "fissionURL"    inCfg BaseUrl
  , HasField' "httpManager"   inCfg HTTP.Manager
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

type LiftErrs =
  '[ Key.Error
   , NoKeyFile
   , ClientError
   , NotRegistered
   , NotFound FilePath
   , NotFound [IPFS.Peer]
   , NotFound Ed25519.SecretKey
   , SomeException
   ]

mkConnected ::
  ( ServerDID (FissionCLI errs inCfg)

  , IsMember YAML.ParseException errs
  , Contains errs        errs
  , Contains LiftErrs    errs

  , Exception (OpenUnion errs)
  , Display   (OpenUnion errs)

  , HasLogFunc                inCfg
  , HasProcessContext         inCfg
  , HasField' "fissionURL"    inCfg BaseUrl
  , HasField' "httpManager"   inCfg HTTP.Manager
  , HasField' "ipfsDaemonVar" inCfg (MVar (Process () () ()))
  )
  => inCfg
  -> IPFS.Timeout -- ^ IPFS timeout in seconds
  -> FissionCLI errs inCfg Config
mkConnected inCfg ipfsTimeout =
  attempt (Key.Store.getAsBytes >>= Ed25519.parseSecretKey) >>= \case
    Left _err -> do
      CLI.Error.put NoKeyFile "Cannot find key. Please run: fission user register"
      raise NoKeyFile

    Right secretKey -> do
      logDebug @Text "Ed25519 key loaded"

      serverDID  <- getServerDID
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

            cliDID = DID
              { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic secretKey
              , method    = Key
              }

            cfg = Config
              { fissionURL  = getField @"fissionURL"  inCfg
              , httpManager = getField @"httpManager" inCfg
              , ..
              }

          Context.run cfg do
            logDebug @Text "Attempting user verification"
            attempt (sendAuthedRequest User.verify) >>= \case
              Left err -> do
                CLI.Error.put err "Not registered. Please run: fission user register"
                raise NotRegistered

              Right _ -> do
                logDebug @Text "User is registered"
                return cfg
