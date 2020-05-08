module Fission.CLI.Prompt.BuildDir
  ( checkBuildDir
  , guessBuildDir
  ) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath

import qualified Data.Text                        as Text

import qualified Fission.Internal.UTF8            as UTF8
import qualified System.Console.ANSI              as ANSI

import           Fission.CLI.Environment.Override as Override
import qualified Fission.CLI.Prompt               as Prompt

-- | Checks user's current build dir by:
--   * recursively checking .fission.yaml
--   * guessing a build dir from common static site generators
--   * prompting the user with that build dir
checkBuildDir ::
  ( MonadIO     m
  , MonadLogger m
  )
  => FilePath
  -> m FilePath
checkBuildDir relPath = do
  absPath <- makeAbsolute relPath

  findEnv absPath >>= \case
    Just (envPath, buildDir) ->
      return $ takeDirectory envPath </> buildDir

    Nothing ->
      guessBuildDir relPath >>= \case
        Nothing ->
          return relPath

        Just guess -> do
          buildDir <- bool relPath guess <$> promptBuildDir guess
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
promptBuildDir :: (MonadIO m, MonadLogger m) => FilePath -> m Bool
promptBuildDir path = do
  UTF8.putText $ "👷 We found a possible build dir: "
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putTextLn $ Text.pack path
  liftIO $ ANSI.setSGR [ANSI.Reset]
  Prompt.reaskYN "🤔 Would you like to upload that instead of the project root? (y/n): "

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
