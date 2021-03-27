module Fission.CLI.YAML
  ( readFile
  , writeFile
  ) where

import qualified Data.Yaml                    as YAML

import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                     as Text

import           Fission.Prelude

import           Fission.CLI.File
import           Fission.Error.NotFound.Types

-- | Writes partial environment to path
writeFile :: (MonadIO m, MonadLogger m, ToJSON a) => FilePath -> a -> m ()
writeFile path contents = do
  logDebug $ "✍️  Writing YAML file to " <> Text.pack (show path)
  forceWrite path $ YAML.encode contents

-- | Decodes file to partial environment
readFile ::
  ( FromJSON a
  , MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => FilePath
  -> m a
readFile path = do
  logDebug $ "👀📖 Reading YAML from " <> Text.pack (show path)
  doesFileExist path >>= \case
    False -> do
      logDebug @Text "🛑 Path does not exist"
      raise $ NotFound @FilePath

    True -> do
      logDebug @Text "🛤️  File path does exist"
      ensureM . liftIO $ YAML.decodeFileEither path
