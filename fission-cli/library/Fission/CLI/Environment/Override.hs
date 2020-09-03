module Fission.CLI.Environment.Override
  ( globalConfig
  , globalDir
  , localConfig
  , localConfigRel
  , decodeFile
  , writeFile
  , writeMerge
  , toFull
  , fromFull
  -- * Reexports
  , module Fission.CLI.Environment.Override.Types
  ) where

import qualified Data.Yaml                              as YAML

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Fission.Prelude

import           Fission.CLI.Environment.Override.Types
import           Fission.CLI.Environment.Types

-- | globalEnv environment in users home
globalConfig :: MonadIO m => m FilePath
globalConfig = do
  path <- globalDir
  return $ path </> "config.yaml"

globalDir :: MonadIO m => m FilePath
globalDir = do
  home <- getHomeDirectory
  return $ home </> ".config" </> "fission"

localConfig :: MonadIO m => m FilePath
localConfig = do
  pwd <- getCurrentDirectory
  return $ pwd </> localConfigRel

localConfigRel :: FilePath
localConfigRel = "fission.yaml"

-- | Decodes file to partial environment
decodeFile ::
  ( MonadIO    m
  , MonadRaise m
  , m `Raises` YAML.ParseException
  )
  => FilePath
  -> m Override
decodeFile path =
  doesFileExist path >>= \case
    False -> return mempty
    True  -> ensureM . liftIO $ YAML.decodeFileEither path

-- | Writes partial environment to path
writeFile :: MonadIO m => FilePath -> Override -> m ()
writeFile path = writeBinaryFileDurable path . YAML.encode

-- | Merge a partial env with an existing env at the path
writeMerge ::
  ( MonadIO    m
  , MonadRaise m
  , m `Raises` YAML.ParseException
  )
  => FilePath
  -> Override
  -> m ()
writeMerge path newEnv = do
  currentEnv <- decodeFile path
  writeFile path $ currentEnv <> newEnv

toFull :: Override -> Environment
toFull Override {..} =
  Environment
    { peers     = peers
    , appURL    = maybeAppURL
    , ignored   = fromMaybe [] maybeIgnored
    , buildDir  = maybeBuildDir
    , serverDID = maybeServerDID
    }

fromFull :: Environment -> Override
fromFull Environment {..} =
  Override
    { maybeUserAuth  = Nothing
    , maybeAppURL    = appURL
    , peers          = peers
    , maybeBuildDir  = buildDir
    , maybeServerDID = serverDID
    , maybeIgnored   = Just ignored
    }
