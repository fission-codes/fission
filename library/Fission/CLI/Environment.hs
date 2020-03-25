-- | Reading and writing local user config values
module Fission.CLI.Environment
  ( init
  , get
  , getPath
  , couldNotRead
  , removeConfigFile
  , getOrRetrievePeer
  , module Fission.CLI.Environment.Class
  ) where

import           Fission.Prelude

import           RIO.Directory
import           RIO.FilePath

import qualified System.FilePath.Glob as Glob
import qualified System.Console.ANSI as ANSI
import           Data.List.NonEmpty  as NonEmpty hiding (init, (<|))

import           Fission.Web.Client
import           Fission.Web.Client.Peers as Peers

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Partial as Env.Partial
import           Fission.CLI.Environment.Partial (globalEnv)

import qualified Fission.Internal.UTF8 as UTF8

import qualified Network.IPFS.Types as IPFS

-- | Initialize the Environment file
init ::
  ( MonadUnliftIO         m
  , MonadLogger           m
  , MonadWebClient        m
  )
  => m ()
init = do
  logDebugN "Initializing config file"
  path <- globalEnv

  Peers.getPeers >>= \case
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right peers -> do
      let
        env = Env.Partial
          { maybeUserAuth = Nothing
          , maybePeers = Just peers
          , maybeIgnored = Just ignoreDefault
          , maybeBuildDir = Nothing
          }
      liftIO <| Env.Partial.write path env
      CLI.Success.putOk "Logged in"

-- | Gets hierarchical environment by recursing through file system
get :: MonadIO m => m Environment
get = do
  partial <- Env.Partial.get
  return <| Env.Partial.toFull partial

-- | Writes env to path, overwriting if necessary
write :: MonadIO m => FilePath -> Environment -> m ()
write path env = Env.Partial.write path <| Env.Partial.fromFull env

-- | Get the path to the Environment file, local or global
getPath :: MonadIO m => Bool -> m FilePath
getPath ofLocal =
  if ofLocal
  then  getCurrentDirectory >>= \dir -> return <| dir </> ".fission.yaml"
  else globalEnv

-- | Create a could not read message for the terminal
couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login\n"

  liftIO <| ANSI.setSGR [ANSI.Reset]

-- | Removes the user's global config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- globalEnv
  try <| removeFile path

-- | Retrieves a Fission Peer from local config
--   If not found we retrive from the network and store
getOrRetrievePeer ::
  ( MonadUnliftIO  m
  , MonadLogger    m
  , MonadWebClient m
  )
  => Environment
  -> m (Maybe IPFS.Peer)
getOrRetrievePeer config =
  case peers config of
    Just prs -> do
      logDebugN "Retrieved Peer from .fission.yaml"
      return <| Just <| head prs

    Nothing ->
      Peers.getPeers >>= \case
        Left err -> do
          logError <| displayShow err
          logDebugN "Unable to retrieve peers from the network"
          return Nothing

        Right peers -> do
          logDebugN "Retrieved Peer from API"
          path <- globalEnv
          write path <| config { peers = Just peers }
          return <| Just <| head peers

ignoreDefault :: IPFS.Ignored
ignoreDefault =
  [ Glob.compile ".fission.yaml"
  , Glob.compile ".env"
  , Glob.compile ".DS_Store"
  ]
