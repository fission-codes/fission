module Fission.CLI.Handler.App.Info (appInfo) where

import qualified Data.Yaml                   as YAML
import qualified RIO.Text                    as Text

import           Fission.Prelude

import           Fission.Error.Types

import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Success as CLI

import           Fission.CLI.App.Environment as Env

appInfo :: forall m .
  ( MonadIO      m
  , MonadLogger  m
  , MonadCleanup m
  , m `Raises` NotFound FilePath
  , m `Raises` YAML.ParseException
  , IsMember YAML.ParseException (Errors m)
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  )
  => m ()
appInfo = do
  attempt Env.read >>= \case
    Right Env {appURL} ->
      CLI.putOk $ "App available at " <> textDisplay appURL

    Left errs ->
      case openUnionMatch errs of
        Just err ->
          CLI.Error.put err . Text.pack $ YAML.prettyPrintParseException err

        Nothing -> do
          CLI.Error.put errs "No app registered yet"
          raise errs
