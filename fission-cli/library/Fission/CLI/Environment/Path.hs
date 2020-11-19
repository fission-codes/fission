module Fission.CLI.Environment.Path
  ( globalBinaryDir
  , globalIPFSBin
  , globalIPFSRepo
  , globalTmpDir
  , globalKeyDir
  , globalUCANDir
  , globalWNFSDir
  ) where

import qualified Network.IPFS.BinPath.Types    as IPFS
import           RIO.FilePath                  ((</>))

import           Fission.Prelude

import           Fission.CLI.Environment.Class

globalBinaryDir :: MonadEnvironment m => m FilePath
globalBinaryDir = do
  global <- getGlobalPath
  return $ global </> "bin"

globalIPFSBin :: MonadEnvironment m => m IPFS.BinPath
globalIPFSBin = do
  binDir <- globalBinaryDir
  return . IPFS.BinPath $ binDir </> "fission-ipfs"

globalIPFSRepo :: MonadEnvironment m => m FilePath
globalIPFSRepo = do
  global <- getGlobalPath
  return $ global </> "ipfs"

globalTmpDir :: MonadEnvironment m => m FilePath
globalTmpDir = do
  dir <- getGlobalPath
  return $ dir </> "tmp"

globalKeyDir :: MonadEnvironment m => m FilePath
globalKeyDir = do
  dir <- getGlobalPath
  return $ dir </> "key"

globalUCANDir :: MonadEnvironment m => m FilePath
globalUCANDir = do
  dir <- getGlobalPath
  return $ dir </> "ucan"

globalWNFSDir :: MonadEnvironment m => m FilePath
globalWNFSDir = do
  dir <- getGlobalPath
  return $ dir </> "wnfs"
