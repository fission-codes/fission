module Fission.CLI.Release (checkLatestRelease) where

import           Data.Versions
import qualified Data.Yaml                               as YAML
import qualified System.Console.ANSI                    as ANSI

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.CLI.Environment                as Env
import qualified Fission.CLI.Meta                       as Meta
import           Fission.CLI.Display.Text
import qualified Fission.CLI.Display.Error       as CLI.Error

import qualified Fission.Internal.UTF8                  as UTF8

import           GitHub (github')
import qualified GitHub
import           GitHub.Data.Releases                   as Releases

checkLatestRelease ::
  ( MonadLogger      m
  , MonadIO          m
  , MonadTime        m
  , MonadCleanup     m
  , MonadEnvironment m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  , Show (OpenUnion (Errors m))
  )
  => m ()
checkLatestRelease = do
  attempt Env.get >>= \case
    Left  err -> CLI.Error.put err "Unable to parse config"
    Right Env {updateChecked} -> do
      now <- currentTime
      let
        nextUpdateTime = addUTCTime 86400 updateChecked
      unless (now < nextUpdateTime) do
        logDebug @Text "Checking for newer versions..."
        possibleVersion <- liftIO $ github' GitHub.latestReleaseR "fission-suite" "fission"
        case possibleVersion of
          Left  _ -> return () -- just ignore errors here.
          Right latestRelease -> do
            Env.update \env -> env {updateChecked = now}
            let
              currentVersion = getVersion $ Meta.version =<< Meta.package
              latestVersion  = getVersion $ Just (Releases.releaseTagName latestRelease)
            if currentVersion < latestVersion
              then colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
                UTF8.putText $ "⚠️  A new version of Fission CLI is available: "
                UTF8.putText $ Releases.releaseTagName latestRelease <> " (aka '" <> Releases.releaseName latestRelease <> "')\n"
              else return () -- no new version

getVersion :: Maybe Text -> Maybe Versioning
getVersion maybeVersion =
  case maybeVersion of
    Nothing -> Nothing
    Just v  ->
      case versioning v of
        Left _ -> Nothing
        Right v' -> return v'
