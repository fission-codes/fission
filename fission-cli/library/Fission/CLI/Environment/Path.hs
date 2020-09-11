module Fission.CLI.Environment.Path
  ( globalBinaryDir
  , globalIPFS
  ) where

import qualified Network.IPFS.BinPath.Types    as IPFS
import           RIO.FilePath                  ((</>))

import           Fission.Prelude

import           Fission.CLI.Environment.Class

globalBinaryDir :: MonadEnvironment m => m FilePath
globalBinaryDir = do
  global <- getGlobalPath
  return $ global </> "bin"

globalIPFS :: MonadEnvironment m => m IPFS.BinPath
globalIPFS = do
  dir <- globalBinaryDir
  return . IPFS.BinPath $ dir </> "ipfs"
