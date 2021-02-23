module Fission.CLI.Release (checkLatestRelease) where

import           Data.Versions
import qualified Data.Yaml                    as YAML
import qualified System.Console.ANSI          as ANSI

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.CLI.Display.Text
import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Meta             as Meta

import qualified Fission.Internal.UTF8        as UTF8

import           GitHub                       (github')
import qualified GitHub
import           GitHub.Data.Releases         as Releases

checkLatestRelease ::
  ( MonadLogger      m
  , MonadIO          m
  , MonadTime        m
  , MonadCleanup     m
  , MonadEnvironment m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => m ()
checkLatestRelease = do
  attempt Env.get >>= \case
    Left  _ ->
      -- Also happens when the CLI isn't setup yet
      logDebug @Text "Unable to parse config"

    Right Env {updateChecked} -> do
      now <- currentTime
      let
        nextUpdateTime = addUTCTime 86400 updateChecked
      unless (now < nextUpdateTime) do
        logDebug @Text "Checking for newer versions..."

        liftIO (github' GitHub.latestReleaseR "fission-suite" "fission") >>= \case
          Left err ->
            logDebug $ show err

          Right latestRelease -> do
            Env.update \env -> env {updateChecked = now}

            let
              currentVersion = getVersion $ Meta.version =<< Meta.package
              latestVersion  = getVersion $ Just (Releases.releaseTagName latestRelease)

            when (currentVersion < latestVersion) do
              colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
                UTF8.putText "⚠️  A new version of Fission CLI is available: "
                UTF8.putTextLn $ Releases.releaseTagName latestRelease <> " (aka '" <> Releases.releaseName latestRelease <> "')"

getVersion :: Maybe Text -> Maybe Versioning
getVersion Nothing = Nothing
getVersion (Just verTxt) =
  case versioning verTxt of
    Left _  -> Nothing
    Right v -> return v
