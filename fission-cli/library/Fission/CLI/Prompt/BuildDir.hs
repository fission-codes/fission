module Fission.CLI.Prompt.BuildDir
  ( guess
  , prompt
  ) where

import qualified RIO.ByteString           as BS
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                 as Text

import qualified Data.ByteString.UTF8     as UTF8
import qualified System.Console.ANSI      as ANSI

import qualified Fission.Internal.UTF8    as UTF8
import           Fission.Prelude

import           Fission.CLI.Display.Text

-- | Prompt the user to see if they'd like to use a build folder instead of the root
prompt ::
  ( MonadIO      m
  , MonadCleanup m
  )
  => FilePath
  -> m FilePath
prompt relPath = do
  fallback <- colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    fallback <- maybe "." identity <$> guess relPath
    UTF8.putText $ "👷 Choose build directory (" <> Text.pack fallback <> "): "
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
            prompt relPath

-- | Check path to see if a possible build folder exists there
guess :: MonadIO m => FilePath -> m (Maybe FilePath)
guess path = foldM (foldGuess path) Nothing (commonPaths path)

-- | Fold function for checking build folder guesses
foldGuess :: MonadIO m => FilePath -> Maybe FilePath -> FilePath -> m (Maybe FilePath)
foldGuess base acc path =
  bool acc (Just path) <$> doesDirectoryExist (base </> path)

-- | Common build folders for static site generators
--   Highest -> Lowest priority (reversed because foldM folds from the left)
commonPaths :: FilePath -> [FilePath]
commonPaths base = reverse $ fmap (base </>)
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
