module Fission.CLI.Auth
  ( getAuth
  , getCachePath
  ) where

import RIO
import RIO.Directory
import RIO.FilePath

import qualified Data.Yaml as Yaml
import           Servant

import Fission.Internal.Orphanage ()

getAuth :: MonadIO m => m (Either Yaml.ParseException BasicAuthData)
getAuth = liftIO . Yaml.decodeFileEither =<< getCachePath

getCachePath :: MonadIO m => m FilePath
getCachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"
