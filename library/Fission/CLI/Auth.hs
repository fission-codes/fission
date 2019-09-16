module Fission.CLI.Auth
  ( cachePath
  , get
  , set
  ) where

import RIO           hiding (set)
import RIO.Directory
import RIO.File
import RIO.FilePath

import qualified Data.Yaml as Yaml
import           Servant

import Fission.Internal.Orphanage.BasicAuthData ()

-- | Retrieve auth from the user's system
get :: MonadIO m => m (Either Yaml.ParseException BasicAuthData)
get = liftIO . Yaml.decodeFileEither =<< cachePath

-- | Write user's auth to a local on-system path
set :: MonadUnliftIO m => BasicAuthData -> m ()
set auth = do
  path <- cachePath
  writeBinaryFileDurable path $ Yaml.encode auth

-- | Absolute path of the auth cache on disk
cachePath :: MonadIO m => m FilePath
cachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"
