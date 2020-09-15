module Fission.CLI.YAML
  ( readFile
  , writeFile
  ) where

import qualified Data.Yaml                    as YAML

import           RIO.Directory
import           RIO.FilePath

import           Fission.Prelude

import           Fission.CLI.File
import           Fission.Error.NotFound.Types

-- | Writes partial environment to path
writeFile ::
  ( MonadIO     m
  , MonadLogger m
  , ToJSON a
  )
  => FilePath
  -> a
  -> m ()
writeFile path = forceWrite path . YAML.encode

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
