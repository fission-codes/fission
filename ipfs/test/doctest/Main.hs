module Main (main) where

import           RIO
import qualified RIO.List as List

import Data.Aeson.Lens
import Data.Yaml

import System.Directory
import System.Directory.Tree
import System.FilePath.Glob (glob)

import Test.DocTest         (doctest)

main :: IO ()
main = do
  let tmp = ".doctest-tmp"
  setup tmp "library"

  source <- glob tmp
  doctest source

  removeDirectoryRecursive tmp

setup :: FilePath -> FilePath -> IO ()
setup tmp src = do
  pwd    <- getCurrentDirectory
  hasTmp <- doesDirectoryExist tmp
  if hasTmp
     then removeDirectoryRecursive tmp
     else return ()

  createDirectory tmp
  exts <- getExts

  (_ :/ files) <- readDirectoryWithL readFileBinary src
  go (pwd <> "/" <> tmp) (header exts) files
  where
    go :: FilePath -> ByteString -> DirTree ByteString -> IO ()
    go dirPath exts' = \case
      Failed { err } ->
        error $ show err

      Dir { name, contents } -> do
        let path = dirPath <> "/" <> name
        createDirectory path
        contents `forM_` go path exts'

      File { name, file } ->
        writeFileBinary (dirPath <> "/" <> name) (exts' <> file)

header :: [ByteString] -> ByteString
header raw = mconcat
  [ "{-# LANGUAGE "
  , mconcat $ List.intersperse ", " raw
  , " #-}\n"
  ]

getExts :: IO [ByteString]
getExts = do
  pkg <- decodeFileThrow "package.yaml" :: IO Value
  let exts = pkg ^. key "default-extensions" . _Array
  return $ extract <$> toList exts
  where
    extract (String txt) = encodeUtf8 txt
    extract _            = error "Malformed package.yaml"
