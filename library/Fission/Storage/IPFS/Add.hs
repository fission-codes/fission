module Fission.Storage.IPFS.Add (walkTest, addDir) where

import           Data.List as List

import           RIO.Directory
import           RIO.FilePath

import qualified System.FilePath.Glob as Glob

import           Fission.Prelude

import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Node.Types as DAG
import           Fission.IPFS.DAG.Link       as DAG.Link
import           Fission.Storage.IPFS.DAG as DAG
import qualified Fission.Storage.IPFS    as IPFS

walkTest = addDir [] "/home/daniel/Projects/fission/hn" >>= \case
  Left _ -> return "ERROR"
  Right cid -> return <| unaddress cid

addDir ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => [Glob.Pattern]
  -> FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
addDir ignored path = doesFileExist path >>= \case
  True -> IPFS.addPath path
  False -> walkDir ignored path

walkDir ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => [Glob.Pattern]
  -> FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
walkDir ignored path = do
  files <- listDirectory path
  let toAdd = removeIgnored ignored files
  results <- mapM (addDir ignored . combine path) toAdd 
  case lefts results of
    [] -> do
      let names = IPFS.Name <$> toAdd
      links <- zipWithM DAG.Link.create (rights results) names
      case lefts links of
        [] -> DAG.putNode Node
              { dataBlock = "CAE="
              , links = rights links
              }
        _  -> return . Left <| firstErr links
    _  -> return . Left <| firstErr results

removeIgnored :: [Glob.Pattern] -> [FilePath] -> [FilePath]
removeIgnored ignored files = filter (not . matchesAny ignored) files

matchesAny :: [Glob.Pattern] -> FilePath -> Bool
matchesAny globs path = any (\x -> Glob.match x path) globs
              
firstErr :: [Either a b] -> a
firstErr = head . lefts
