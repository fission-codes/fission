-- | Reading and writing local user config values
module Fission.CLI.Environment
  ( init
  , get
  , put
  , update
  , getOrRetrievePeers
  , absPath
  , fetchServerDID

  -- * Reexport

  , module Fission.CLI.Environment.Class
  , module Fission.CLI.Environment.Types
  ) where

import qualified Data.ByteString.Char8         as BS8
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Yaml                     as YAML
import           RIO.FilePath

import qualified Network.DNS                   as DNS
import           Servant.Client

import qualified Network.IPFS.Types            as IPFS

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.User.DID.Types
import           Fission.User.Username.Types
import           Fission.Web.Client

import qualified Fission.CLI.Display.Error     as CLI.Error
import           Fission.CLI.Environment.Path  as Path
import           Fission.CLI.IPFS.Peers        as Peers
import qualified Fission.CLI.YAML              as YAML

-- Reexports

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Types

-- | Initialize the Environment file
init ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadLogger      m
  , MonadWebClient   m

  , MonadCleanup m
  , m `Raises` ClientError
  , m `Raises` DNS.DNSError
  , m `Raises` NotFound DID
  , Show (OpenUnion (Errors m))
  )
  => Username
  -> BaseUrl
  -> m ()
init username fissionURL = do
  logDebug @Text "Initializing config file"

  attempt Peers.getPeers >>= \case
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right nonEmptyPeers -> do
      serverDID      <- fetchServerDID fissionURL
      envPath        <- absPath
      signingKeyPath <- Path.getSigningKeyPath

      envPath `YAML.writeFile` Env
        { peers          = NonEmpty.toList nonEmptyPeers
        , ignored        = []
        , signingKeyPath
        , serverDID
        , username
        , updateChecked  = fromSeconds 0
        }

-- | Gets hierarchical environment by recursing through file system
get ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadLogger      m
  , MonadRaise       m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => m Env
get = do
  logDebug @Text "Reading global config.yaml"
  path <- absPath
  env  <- YAML.readFile path
  return env

put :: (MonadEnvironment m, MonadIO m, MonadLogger m) => Env -> m ()
put env = do
  envPath <- absPath
  envPath `YAML.writeFile` env

update ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadLogger      m
  , MonadRaise       m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => (Env -> Env)
  -> m ()
update updater = do
  env <- get
  put $ updater env

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

fetchServerDID ::
  ( MonadIO     m
  , MonadRaise  m
  , MonadLogger m
  , m `Raises` DNS.DNSError
  , m `Raises` NotFound DID
  )
  => BaseUrl
  -> m DID
fetchServerDID fissionURL = do
  rs      <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
  let url = BS8.pack $ "_did." <> baseUrlHost fissionURL

  logDebug $ "No cached server DID. Fetching from " <> decodeUtf8Lenient url

  liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
    Left errs -> do
      CLI.Error.put errs "Unable to find Fission's ID online"
      raise errs

    Right [] -> do
      CLI.Error.put (NotFound @DID) $ "No TXT record at " <> decodeUtf8Lenient url
      raise $ NotFound @DID

    Right (didTxt : _) ->
      case eitherDecodeStrict ("\"" <> didTxt <> "\"") of
        Left errs -> do
          CLI.Error.put errs "Unable to find Fission's ID online"
          raise $ NotFound @DID

        Right serverDID -> do
          logDebug $ "DID retrieved " <> textDisplay serverDID
          return serverDID
