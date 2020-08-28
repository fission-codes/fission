module Fission.CLI.Prompt.BuildDir
  ( checkBuildDir
  , guessBuildDir
  , promptBuildDir
  ) where

import qualified RIO.ByteString                   as BS
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                         as Text

import qualified Data.ByteString.UTF8             as UTF8
import qualified System.Console.ANSI              as ANSI

import qualified Fission.Internal.UTF8            as UTF8
import           Fission.Prelude

import           Fission.CLI.Display.Text
import           Fission.CLI.Environment.Override as Override

-- | Checks user's current build dir by:
--   * recursively checking .fission.yaml
--   * guessing a build dir from common static site generators
--   * prompting the user with that build dir
checkBuildDir ::
  ( MonadIO      m
  , MonadCleanup m
  )
  => FilePath
  -> m FilePath
checkBuildDir relPath = do
  absPath <- makeAbsolute relPath

  findEnv absPath >>= \case
    Just (envPath, buildDir) ->
      return $ takeDirectory envPath </> buildDir

    Nothing -> do
      buildDir <- promptBuildDir relPath
      let updated = mempty { maybeBuildDir = Just buildDir }

      Override.writeMerge (absPath </> ".fission.yaml") updated
      return buildDir

-- | Find the closests '.fission.yaml' that contains a build directory,
--   and return the location of both the env and the build directory
findEnv ::
  MonadIO m
  => FilePath
  -> m (Maybe (FilePath, FilePath)) -- ^ (closest env, build directory)
findEnv path = do
  Override.findRecurse (isJust . maybeBuildDir) path <&> \case
    Nothing ->
      Nothing

    Just (closestEnvPath, Override {..}) ->
      case maybeBuildDir of
        Nothing       -> Nothing
        Just buildDir -> Just (closestEnvPath, buildDir)

-- | Prompt the user to see if they'd like to use a build folder instead of the root
promptBuildDir ::
  ( MonadIO      m
  , MonadCleanup m
  )
  => FilePath
  -> m FilePath
promptBuildDir relPath = do
  fallback <- colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    fallback <- maybe "." identity <$> guessBuildDir relPath
    UTF8.putText $ "👷 Build directory (" <> Text.pack fallback <> "): "
    return fallback

  BS.getLine >>= \case
    "" ->
      return fallback

    bsPath ->
      let
        path = UTF8.toString bsPath
      in
        doesDirectoryExist path >>= \case
          True ->
            return $ UTF8.toString bsPath

          False -> do
            UTF8.putTextLn $ Text.pack path <> " does not exist"
            promptBuildDir relPath

-- | Check path to see if a possible build folder exists there
guessBuildDir :: MonadIO m => FilePath -> m (Maybe FilePath)
guessBuildDir path = foldM (foldGuess path) Nothing (buildGuesses path)

-- | Fold function for checking build folder guesses
foldGuess :: MonadIO m => FilePath -> Maybe FilePath -> FilePath -> m (Maybe FilePath)
foldGuess base acc path =
  bool acc (Just path) <$> doesDirectoryExist (base </> path)

-- | Common build folders for static site generators
--   Highest -> Lowest priority (reversed because foldM folds from the left)
buildGuesses :: FilePath -> [FilePath]
buildGuesses base = reverse $ fmap (base </>)
  [ "_site"         -- jekyll, hakyll, eleventy
  , "site"          -- fairly common
  , "build"         -- create-react-app, metalsmith, middleman
  , "dist"          -- nuxt
  , "public"        -- gatsby, hugo
  , "output"        -- pelican
  , "out"           -- hexo
  , "website/build" -- docusaurus
  , "docs"          -- many others
  ]
