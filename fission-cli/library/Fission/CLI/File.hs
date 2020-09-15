module Fission.CLI.File (forceWrite) where

import qualified RIO.Text        as Text

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Fission.Prelude

forceWrite ::
  ( MonadIO     m
  , MonadLogger m
  )
  => FilePath
  -> ByteString
  -> m ()
forceWrite path bs = do
  logDebug $ "Writing to " <> Text.pack path
  createDirectoryIfMissing True $ dropFileName path
  writeBinaryFileDurableAtomic path bs
