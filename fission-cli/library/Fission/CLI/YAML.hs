module Fission.CLI.YAML
  ( readFile
  , writeFile
  ) where

import qualified Data.Yaml                    as YAML

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Fission.Error.NotFound.Types
import           Fission.Prelude

-- | Writes partial environment to path
writeFile :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeFile path = writeBinaryFileDurable path . YAML.encode

-- | Decodes file to partial environment
readFile ::
  ( FromJSON a
  , MonadIO    m
  , MonadRaise m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => FilePath
  -> m a
readFile path =
  doesFileExist path >>= \case
    False -> raise $ NotFound @FilePath
    True  -> ensureM . liftIO $ YAML.decodeFileEither path
