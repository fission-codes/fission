-- | Reading and writing local user config values
module Fission.CLI.Environment
  ( init
  , get
  , getOrRetrievePeers

  -- * Reexport

  , module Fission.CLI.Environment.Class
  , module Fission.CLI.Environment.Types
  ) where

import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Yaml                       as YAML

import           RIO.FilePath

import           Servant.Client

import qualified Network.IPFS.Types              as IPFS

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import           Fission.Web.Client
import           Fission.Web.Client.Peers        as Peers

import           Fission.CLI.Environment.Path    as Path

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.YAML                as YAML

-- Reexports

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Types

-- | Initialize the Environment file
init ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadLogger      m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup m
  , m `Raises` ClientError
  , Show (OpenUnion (Errors m))
  )
  => m ()
init = do
  logDebug @Text "Initializing config file"

  attempt Peers.getPeers >>= \case
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right nonEmptyPeers -> do
      serverDID      <- getServerDID
      envPath        <- absPath
      signingKeyPath <- Path.getSigningKeyPath

      envPath `YAML.writeFile` Env
        { peers          = NonEmpty.toList nonEmptyPeers
        , ignored        = []
        , signingKeyPath
        , serverDID
        }

-- | Gets hierarchical environment by recursing through file system
get ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadRaise       m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => m Env
get = YAML.readFile =<< absPath

-- | Retrieves a Fission Peer from local config
--   If not found we retrive from the network and store
getOrRetrievePeers ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadWebClient   m
  , MonadEnvironment m

  , MonadCleanup m
  , m `Raises` ClientError
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  -- , Show (OpenUnion (Errors m))
  )
  => Env
  -> m [IPFS.Peer]
getOrRetrievePeers Env {peers = []} = do
  nonEmptyPeers <- Peers.getPeers
  logDebug $ "Retrieved Peers from API, and writing to ~/.fission.yaml: " <> textShow nonEmptyPeers

  path    <- absPath
  current <- YAML.readFile @Env path

  let
    peers' = peers current <> NonEmpty.toList nonEmptyPeers

  YAML.writeFile path current { peers = peers' }
  return peers'

getOrRetrievePeers Env {peers} = do
  logDebug $ "Retrieved Peers from .fission.yaml: " <> textShow peers
  return peers

absPath :: MonadEnvironment m => m FilePath
absPath = do
  path <- getGlobalPath
  return $ path </> "config.yaml"
