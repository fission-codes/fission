module Fission.CLI.Release (checkLatestRelease) where

import           Data.Versions
import qualified System.Console.ANSI                    as ANSI

import           Fission.Prelude

import qualified Fission.CLI.Meta                       as Meta
import           Fission.CLI.Display.Text
import qualified Fission.Internal.UTF8                  as UTF8

import           GitHub (github')
import qualified GitHub
import           GitHub.Data.Releases                   as Releases

checkLatestRelease ::
  ( MonadLogger  m
  , MonadIO      m
  , MonadCleanup m
  )
  => m ()
checkLatestRelease = do
  -- check "last checked" timestamp to see if we need to check again
  logDebug @Text "Checking for newer versions..."

  possibleVersion <- liftIO $ github' GitHub.latestReleaseR "fission-suite" "fission"
  case possibleVersion of
    Left  _ -> return () -- just ignore errors here.
    Right latestRelease -> do
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
        Right version -> return version
