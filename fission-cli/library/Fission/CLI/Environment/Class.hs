module Fission.CLI.Environment.Class (MonadEnvironment (..)) where

import           RIO.Directory

import           Fission.Prelude

import           Fission.Internal.Mock

-- | A monad representing access to the configurable environment
class Monad m => MonadEnvironment m where
  getGlobalPath :: m FilePath -- ^ Must be the absolute path

instance MonadEnvironment (Mock effs cfg) where
  getGlobalPath = do
    str <- Mock . liftIO $ generate arbitrary -- Skipping RunIO effect because internal
    let path = "/tmp/" <> str
    Mock . liftIO $ createDirectoryIfMissing True path
    return path
