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
  , deleteHomeAuth
  , module Fission.CLI.Environment.Override.Types
  ) where

import qualified Data.Yaml as YAML

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Servant.API

import           Fission.Prelude hiding (decode)

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Override.Types

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m Override
get = getRecurse =<< getCurrentDirectory

getRecurse :: MonadIO m => FilePath -> m Override
getRecurse "/" = do
  root <- decode "/.fission.yaml"
  home <- decode =<< globalEnv
  return (home <> root)

getRecurse path = do
  parent <- getRecurse $ takeDirectory path
  curr <- decode (path </> ".fission.yaml")
  return (parent <> curr)

-- | Recurses up to user root to find an env with basic auth data
findBasicAuth :: MonadIO m => m (Maybe BasicAuthData)
findBasicAuth = do
  currDir <- getCurrentDirectory
  findRecurse (isJust . maybeUserAuth) currDir <&> \case
    Just (_, env) -> maybeUserAuth env
    Nothing       -> Nothing

-- | Recurses up to user root to find a env that satisfies the 'finder' predicate
findRecurse ::
  MonadIO m
  => (Override -> Bool)
  -> FilePath
  -> m (Maybe (FilePath, Override))
findRecurse pred currentPath = do
  let filepath = currentPath </> ".fission.yaml"
  overrideEnv <- decode filepath
 
  if | pred overrideEnv ->
         return $ Just (filepath, overrideEnv)
        
     | currentPath == "/" -> do
         globalPath <- globalEnv
         globalEnv' <- decode globalPath
         return if pred globalEnv'
                  then Just (globalPath, globalEnv')
                  else Nothing
                     
     | otherwise ->
         findRecurse pred $ takeDirectory currentPath

-- | Decodes file to partial environment
decode :: MonadIO m => FilePath -> m Override
decode path = liftIO (either mempty identity <$> YAML.decodeFileEither path)

-- | Writes partial environment to path
write :: MonadIO m => FilePath -> Override -> m ()
write path env = writeBinaryFileDurable path $ YAML.encode env

-- | Merge a partial env with an existing env at the path
writeMerge :: MonadIO m => FilePath -> Override -> m ()
writeMerge path newEnv = do
  currentEnv <- decode path
  write path $ currentEnv <> newEnv

-- | globalEnv environment in users home
globalEnv :: MonadIO m => m FilePath
globalEnv = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

toFull :: Override -> Environment
toFull Override {..} = do
  Environment
    { peers     = peers
    , appURL    = maybeAppURL
    , ignored   = fromMaybe [] $ maybeIgnored
    , buildDir  = maybeBuildDir
    , serverDID = maybeServerDID
    }

fromFull :: Environment -> Override
fromFull Environment {..} = Override
  { maybeUserAuth  = Nothing
  , maybeAppURL    = appURL
  , peers          = peers
  , maybeBuildDir  = buildDir
  , maybeServerDID = serverDID
  , maybeIgnored   = Just ignored
  }

-- | Deletes user_auth from env at ~/.fission.yaml
deleteHomeAuth :: MonadIO m => m ()
deleteHomeAuth = do
  path       <- globalEnv
  currentEnv <- decode path
  write path $ currentEnv { maybeUserAuth = Nothing }
