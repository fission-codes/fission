module Fission.CLI.Environment.Path
  ( globalBinaryDir
  , globalIPFS
  ) where

import           RIO.FilePath                  ((</>))

import           Fission.Prelude

import           Fission.CLI.Environment.Class

globalBinaryDir :: MonadEnvironment m => m FilePath
globalBinaryDir = do
  global <- getGlobalPath
  return $ global </> "bin"

globalIPFS :: MonadEnvironment m => m FilePath
globalIPFS = do
  dir <- globalBinaryDir
  return $ dir </> "ipfs"
