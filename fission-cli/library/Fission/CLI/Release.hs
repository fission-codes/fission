module Fission.CLI.Release (checkLatestRelease) where

import           Fission.Prelude

import           GitHub (github')
import qualified GitHub
import           GitHub.Data.Releases                           as Releases

checkLatestVersion ::
  ( MonadLogger m
  , MonadIO     m
  )
  => m ()
checkLatestRelease = do
  -- check "last checked" timestamp to see if we need to check again
  logInfo @Text "Checking for newer versions..."

  possibleVersion <- liftIO $ github' GitHub.latestReleaseR "fission-suite" "fission"
  case possibleVersion of
    Left  _ -> return () -- just ignore errors here.
    Right latestVersion -> do
      -- check latest version against current version from Meta.package
      logInfo $ "latest version is: " <> Releases.releaseTagName latestVersion
      return ()
