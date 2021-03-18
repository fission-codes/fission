module Fission.CLI.Release (checkLatestRelease) where

import qualified System.Console.ANSI          as ANSI

import qualified RIO.Text                     as Text

import qualified Data.Version                 as Version
import qualified Data.Versions                as Versions
import qualified Data.Yaml                    as YAML

import           GitHub                       (github')
import qualified GitHub
import           GitHub.Data.Releases         as Releases

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.Internal.UTF8        as UTF8

import           Fission.CLI.Display.Text
import           Fission.CLI.Environment      as Env

import qualified Paths_fission_cli            as CLI

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
        logDebug @Text "👀☁️  Checking for newer versions..."

        liftIO (github' GitHub.latestReleaseR "fission-suite" "fission") >>= \case
          Left err ->
            logDebug $ show err

          Right latestRelease -> do
            Env.update \env -> env {updateChecked = now}

            let
              currentVersion = getVersion . Text.pack $ Version.showVersion CLI.version
              latestVersion  = getVersion $ Releases.releaseTagName latestRelease

            when (currentVersion < latestVersion) do
              colourized [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow] do
                UTF8.putText "⚠️  A new version of Fission CLI is available: "
                UTF8.putTextLn $ Releases.releaseTagName latestRelease <> " (AKA '" <> Releases.releaseName latestRelease <> "')"

getVersion :: Text -> Maybe Versions.Versioning
getVersion verTxt =
  case Versions.versioning verTxt of
    Left _  -> Nothing
    Right v -> return v
