module Fission.CLI.Prompt.BuildDir (checkBuildDir) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath

import           Data.Text as T (pack)

import qualified System.Console.ANSI   as ANSI
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Prompt      as Prompt
import qualified Fission.CLI.Environment.Partial as Env.Partial
import           Fission.CLI.Environment.Partial.Types as Env

-- | Checks user's current build dir by:
--   * recursively checking .fission.yaml
--   * guessing a build dir from common static site generators
--   * prompting the user with that build dir
checkBuildDir ::
  ( MonadIO m, MonadLogger m )
  => FilePath
  -> m FilePath
checkBuildDir relPath = do
  absPath <- makeAbsolute relPath
  findEnv absPath >>= \case
    Just (envPath, buildDir) ->
      return <| (takeDirectory envPath) </> buildDir
    Nothing -> guessBuildDir relPath >>= \case
      Nothing -> return relPath
      Just guess -> do
        buildDir <- promptBuildDir guess >>= \case
          True -> return guess
          False -> return relPath
        let
          updated = (mempty Env.Partial) { maybeBuildDir = Just buildDir }
        Env.Partial.writeMerge (absPath </> ".fission.yaml") updated
        return buildDir

-- | Find the closests .fission.yaml that contains a build_dir in it, and return the location of the env & build_dir
findEnv :: ( MonadIO m ) => FilePath -> m (Maybe (FilePath, FilePath))
findEnv path = Env.Partial.findRecurse (isJust . maybeBuildDir) path >>= \case
  Nothing -> return Nothing
  Just (envPath, env) ->
    case maybeBuildDir env of
      Nothing -> return Nothing
      Just buildDir -> return <| Just (envPath, buildDir)

-- | Prompt the user to see if they'd like to use a build folder instead of the root
promptBuildDir :: ( MonadIO m, MonadLogger m ) => FilePath -> m Bool
promptBuildDir path = do
  UTF8.putText <| "👷 We found a possible build dir: "
  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putTextLn <| T.pack path
  liftIO <| ANSI.setSGR [ANSI.Reset]
  -- \xD83E\xDD14 == 🤔 (will be available in the next version of the parser)
  Prompt.reaskYN "\xD83E\xDD14 Would you like to upload that instead of the project root? (y/n): "

-- | Check path to see if a possible build folder exists there
guessBuildDir :: ( MonadIO m ) => FilePath -> m (Maybe FilePath)
guessBuildDir path = foldM (foldGuess path) Nothing (buildGuesses path)

-- | Fold function for checking build folder guesses
foldGuess :: MonadIO m => FilePath -> Maybe FilePath -> FilePath -> m (Maybe FilePath)
foldGuess base acc path = (doesDirectoryExist <| combine base path) >>= \case
  True -> return <| Just path
  False -> return acc

-- | Common build folders for static site generators
--   Highest -> Lowest priority (reversed because foldM folds from the left)
buildGuesses :: FilePath -> [FilePath]
buildGuesses base = reverse <| map (combine base)
  [ "_site" -- jekyll, hakyll, eleventy
  , "site" -- forgot which
  , "build" -- create-react-app, metalsmith, middleman
  , "dist" -- nuxt
  , "public" -- gatsby, hugo
  , "output" -- pelican
  , "out" -- hexo
  , "website/build" -- docusaurus
  , "docs" -- many others
  ]
