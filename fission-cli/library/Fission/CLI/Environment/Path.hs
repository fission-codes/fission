module Fission.CLI.Environment.Path
  ( globalBinaryDir
  , globalIPFS
  , globalTmpDir
  , globalKeyDir
  , signingKeyPath
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

globalTmpDir :: MonadEnvironment m => m FilePath
globalTmpDir = do
  dir <- globalBinaryDir
  return $ dir </> "tmp"

globalKeyDir :: MonadEnvironment m => m FilePath
globalKeyDir = do
  dir <- globalBinaryDir
  return $ dir </> "key"

signingKeyPath :: MonadIO m => m FilePath
signingKeyPath = do
  path <- globalKeyDir
  return $ path </> "machine_id.ed25519"
