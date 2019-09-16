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

get :: MonadIO m => m (Either Yaml.ParseException BasicAuthData)
get = liftIO . Yaml.decodeFileEither =<< cachePath

set :: MonadUnliftIO m => BasicAuthData -> m ()
set auth = do
  path <- cachePath
  writeBinaryFileDurable path $ Yaml.encode auth

cachePath :: MonadIO m => m FilePath
cachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"
