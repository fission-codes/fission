module Fission.CLI.Environment.Partial
  ( get
  , findBasicAuth
  , findRecurse
  , decode
  , write
  , writeMerge
  , globalEnv
  , toFull
  , fromFull
  , updatePeers
  , deleteHomeAuth
  ) where

import           Fission.Prelude hiding (decode)

import           Servant.API

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified Data.Yaml as YAML
import           Data.List.NonEmpty as NonEmpty hiding ((<|))

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Env

import qualified Network.IPFS.Types as IPFS

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m Env.Partial
get = getRecurse =<< getCurrentDirectory

getRecurse :: MonadIO m => FilePath -> m Env.Partial
getRecurse "/" = do
  root <- decode <| "/.fission.yaml"
  home <- decode =<< globalEnv
  return <| home <> root

getRecurse path = do
  parent <- getRecurse <| takeDirectory path
  curr <- decode <| path </> ".fission.yaml"
  return <| parent <> curr

-- | Recurses up to user root to find an env with basic auth data
findBasicAuth :: MonadIO m => m (Maybe BasicAuthData)
findBasicAuth = do
  currDir <- getCurrentDirectory
  findRecurse (isJust . maybeUserAuth) currDir >>= \case
    Nothing -> return Nothing
    Just (_, env) -> return <| maybeUserAuth env

-- | Recurses up to user root to find a env that satisfies function "f"
findRecurse ::
  MonadIO m
  => (Env.Partial -> Bool)
  -> FilePath
  -> m (Maybe (FilePath, Env.Partial))
findRecurse f p = do
  let filepath = p </> ".fission.yaml"
  partial <- decode filepath
  case (f partial, p) of
    -- if found, return
    (True, _) -> return <| Just (filepath, partial)
    -- if at root, check globalEnv (home dir)
    -- necessary for WSL
    (_, "/")  -> do
      globalPath <- globalEnv
      global <- decode globalPath
      if f global
        then return <| Just (globalPath, global)
        else return Nothing
    -- else recurse
    _         -> findRecurse f <| takeDirectory p

-- | Decodes file to partial environment
decode :: MonadIO m => FilePath -> m Env.Partial
decode path = liftIO <| YAML.decodeFileEither path >>= \case
  Left _ -> return <| mempty Env.Partial
  Right env -> return env

-- | Writes partial environment to path
write :: MonadIO m => FilePath -> Env.Partial -> m ()
write path env = writeBinaryFileDurable path <| YAML.encode env

-- | Merges partial env with the env at the path and overwrites
writeMerge :: MonadIO m => FilePath -> Env.Partial -> m ()
writeMerge path newEnv = do
  currEnv <- decode path
  writeBinaryFileDurable path <| YAML.encode <| currEnv <> newEnv

-- | globalEnv environment in users home
globalEnv :: MonadIO m => m FilePath
globalEnv = do
  home <- getHomeDirectory
  return <| home </> ".fission.yaml"

toFull :: Env.Partial -> Environment
toFull partial =
  Environment
    { peers = maybePeers partial
    , ignored = fromMaybe [] <| maybeIgnored partial
    , buildDir = maybeBuildDir partial
    }

fromFull :: Environment -> Env.Partial
fromFull env = Env.Partial
  { maybeUserAuth = Nothing
  , maybePeers = peers env
  , maybeIgnored = Just <| ignored env
  , maybeBuildDir = buildDir env
  }

updatePeers :: Env.Partial -> [IPFS.Peer] -> Env.Partial
updatePeers env peers = env
  { maybePeers = Just (NonEmpty.fromList peers) }

-- | Deletes user_auth from env at ~/.fission.yaml
deleteHomeAuth :: MonadIO m => m ()
deleteHomeAuth = do
  path <- globalEnv
  currEnv <- decode path
  write path <| currEnv { maybeUserAuth = Nothing }
