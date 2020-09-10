module Fission.CLI.Handler.App.Info (appInfo) where

import qualified Data.Yaml                   as YAML

import           Fission.Prelude

import           Fission.Error.Types
import           Fission.URL.Types

import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Success as CLI

import           Fission.CLI.Environment     as Environment

appInfo ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadCleanup     m
  , m `Raises` NotFound URL
  , m `Raises` YAML.ParseException
  )
  => m ()
appInfo = do
  Environment {appURL} <- Environment.get

  case appURL of
    Nothing -> do
      let err = NotFound @URL
      CLI.Error.put err "No app registered yet"
      raise err

    Just url ->
      CLI.putOk $ "App available at " <> textDisplay url
