module Fission.CLI.Auth
  ( cachePath
  , get
  , set
  , withAuth
  ) where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified System.Console.ANSI as ANSI
import qualified Fission.Emoji           as Emoji

import qualified Data.Yaml as YAML
import           Servant

import           Fission.CLI.Loader -- TODO move putText to proper module
import Fission.Internal.Orphanage.BasicAuthData ()
import Fission.Internal.Constraint

-- | Retrieve auth from the user's system
get :: MonadIO m => m (Either YAML.ParseException BasicAuthData)
get = liftIO . YAML.decodeFileEither =<< cachePath

-- | Write user's auth to a local on-system path
set :: MonadUnliftIO m => BasicAuthData -> m ()
set auth = do
  path <- cachePath
  writeBinaryFileDurable path $ YAML.encode auth

-- | Absolute path of the auth cache on disk
cachePath :: MonadIO m => m FilePath
cachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

withAuth :: (MonadRIO cfg m, HasLogFunc cfg) => (BasicAuthData -> m ()) -> m ()
withAuth action = get >>= \case
  Right auth ->
    action auth

  Left err -> do
    logError $ displayShow err
    liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
    putText $ Emoji.prohibited <> " Unable to read credentials. Try logging in with "
    liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    putText "fission-cli login"
