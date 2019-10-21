module Fission.CLI.Auth
  ( cachePath
  , get
  , withAuth
  , write
  , couldNotRead
  , removeConfigFile
  ) where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified System.Console.ANSI as ANSI

import qualified Data.Yaml as YAML
import           Servant
import           Servant.Client

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.BasicAuthData ()
import qualified Fission.Internal.UTF8 as UTF8

-- | Retrieve auth from the user's system
get :: MonadIO m => m (Either YAML.ParseException BasicAuthData)
get = liftIO . YAML.decodeFileEither =<< cachePath

-- | Write user's auth to a local on-system path
write :: MonadUnliftIO m => BasicAuthData -> m ()
write auth = do
  path <- cachePath
  writeBinaryFileDurable path $ YAML.encode auth

-- | Absolute path of the auth cache on disk
cachePath :: MonadIO m => m FilePath
cachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login"

  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Removes the users config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- cachePath
  try $ removeFile path

withAuth :: MonadRIO   cfg m
         => HasLogFunc cfg
         => (BasicAuthData -> m (Either ClientError a))
         -> m (Either SomeException a)
withAuth action = get >>= \case
  Right auth ->
    action auth >>= pure . \case
      Right result -> Right result
      Left err -> Left $ toException err

  Left err -> do
    logError $ displayShow err
    couldNotRead
    return . Left $ toException err
