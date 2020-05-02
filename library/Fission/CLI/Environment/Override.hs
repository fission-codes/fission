module Fission.CLI.Environment.Override
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


import qualified Network.IPFS.Types as IPFS
import           Servant.API

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified Data.Yaml as YAML
import           Data.List.NonEmpty as NonEmpty

import           Fission.Prelude hiding (decode)

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Override.Types as Env


-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m Env.Override
get = getRecurse =<< getCurrentDirectory

getRecurse :: MonadIO m => FilePath -> m Env.Override
getRecurse "/" = do
  root <- decode $ "/.fission.yaml"
  home <- decode =<< globalEnv
  return $ home <> root

getRecurse path = do
  parent <- getRecurse $ takeDirectory path
  curr <- decode $ path </> ".fission.yaml"
  return $ parent <> curr

-- | Recurses up to user root to find an env with basic auth data
findBasicAuth :: MonadIO m => m (Maybe BasicAuthData)
findBasicAuth = do
  currDir <- getCurrentDirectory
  findRecurse (isJust . maybeUserAuth) currDir >>= \case
    Nothing -> return Nothing
    Just (_, env) -> return $ maybeUserAuth env

-- | Recurses up to user root to find a env that satisfies function "f"
findRecurse ::
  MonadIO m
  => (Env.Override -> Bool)
  -> FilePath
  -> m (Maybe (FilePath, Env.Override))
findRecurse f p = do
  let filepath = p </> ".fission.yaml"
  partial <- decode filepath
  case (f partial, p) of
    -- if found, return
    (True, _) -> return $ Just (filepath, partial)
    -- if at root, check globalEnv (home dir)
    -- necessary for WSL
    (_, "/")  -> do
      globalPath <- globalEnv
      global <- decode globalPath
      if f global
        then return $ Just (globalPath, global)
        else return Nothing
    -- else recurse
    _         -> findRecurse f $ takeDirectory p

-- | Decodes file to partial environment
decode :: MonadIO m => FilePath -> m Env.Override
decode path = liftIO $ YAML.decodeFileEither path >>= \case
  Left _ -> return $ mempty Env.Override
  Right env -> return env

-- | Writes partial environment to path
write :: MonadIO m => FilePath -> Env.Override -> m ()
write path env = writeBinaryFileDurable path $ YAML.encode env

-- | Merges partial env with the env at the path and overwrites
writeMerge :: MonadIO m => FilePath -> Env.Override -> m ()
writeMerge path newEnv = do
  currEnv <- decode path
  writeBinaryFileDurable path $ YAML.encode $ currEnv <> newEnv

-- | globalEnv environment in users home
globalEnv :: MonadIO m => m FilePath
globalEnv = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

toFull :: Env.Override -> Environment
toFull Env.Override {..} =
  Environment
    { peers    = NonEmpty.fromList peers
    , ignored  = fromMaybe [] maybeIgnored
    , buildDir = fromMaybe "." maybeBuildDir
    }

fromFull :: Environment -> Env.Override
fromFull Environment {..} = Env.Override
  { maybeUserAuth = Nothing
  , peers         = NonEmpty.toList peers
  , maybeIgnored  = Just ignored
  , maybeBuildDir = Just buildDir
  }

updatePeers :: Env.Override -> [IPFS.Peer] -> Env.Override
updatePeers env@Env.Override {peers} newPeers = env { peers = peers <> newPeers }

-- | Deletes user_auth from env at ~/.fission.yaml
deleteHomeAuth :: MonadIO m => m ()
deleteHomeAuth = do
  path    <- globalEnv
  currEnv <- decode path
  write path $ currEnv { maybeUserAuth = Nothing }
