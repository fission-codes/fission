module Fission.CLI.JSON
  ( readFile
  , writeFile
  ) where

import qualified RIO.ByteString.Lazy          as Lazy
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                     as Text

import           Fission.Prelude

import           Fission.CLI.File
import           Fission.Error.NotFound.Types
import qualified Fission.JSON                 as JSON

-- | Writes partial environment to path
writeFile ::
  ( MonadIO     m
  , MonadLogger m
  , ToJSON a
  )
  => FilePath
  -> a
  -> m ()
writeFile path contents = do
  logDebug $ "✍️  Writing JSON file to " <> Text.pack (show path)
  forceWrite path . Lazy.toStrict $ encode contents

-- | Decodes file to partial environment
readFile ::
  ( FromJSON    a
  , MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` JSON.Error
  , m `Raises` NotFound FilePath
  )
  => FilePath
  -> m a
readFile path = do
  logDebug $ "👀📖 Reading JSON from " <> Text.pack (show path)
  doesFileExist path >>= \case
    False -> do
      logDebug @Text "🛑 File path does not exist"
      raise $ NotFound @FilePath

    True -> do
      logDebug @Text "🛤️  File path does exist"
      ensureM $ liftIO (JSON.betterError <$> eitherDecodeFileStrict' path)
