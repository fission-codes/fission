module Fission.CLI.IPFS.Ignore
  ( writeTmp
  , module Fission.CLI.IPFS.Ignore.Class
  ) where

import           RIO.File
import           RIO.FilePath
import qualified RIO.Text                      as Text

import           Fission.Prelude

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Path
import           Fission.CLI.IPFS.Ignore.Class

writeTmp ::
  ( MonadIPFSIgnore  m
  , MonadEnvironment m
  , MonadIO          m
  )
  => FilePath
  -> m FilePath
writeTmp name = do
  ignores <- getIgnoredFiles
  tmpDir  <- globalTmpDir

  let
    ignoresTxt = encodeUtf8 $ Text.intercalate "\n" ignores
    path = tmpDir </> name

  path `writeBinaryFileDurableAtomic` ignoresTxt
  return path
