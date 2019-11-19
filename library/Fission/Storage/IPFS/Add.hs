module Fission.Storage.IPFS.Add (walk, getSize) where

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List as List

import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Directory
import           RIO.FilePath

import           Fission.Prelude
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Node.Types as DAG
import           Fission.IPFS.DAG.Link.Types as DAG
import           Fission.Storage.IPFS.DAG as DAG
import qualified Fission.Storage.IPFS    as IPFS

walk ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
walk path = doesFileExist path >>= \case
  True -> IPFS.addPath path
  False -> addDir path

addDir ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
addDir path = do
  files <- listDirectory path
  results <- mapM (walk . combine path) files 
  case lefts results of
    [] -> do
      let names = IPFS.Name <$> files
      links <- zipWithM createLink (rights results) names
      case lefts links of
        [] -> DAG.putNode Node
              { dataBlock = "CAE="
              , links = rights links
              }
        _  -> return . Left <| firstErr links
    _  -> return . Left <| firstErr results
              
firstErr :: [Either a b] -> a
firstErr = List.head . lefts

getSize ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => IPFS.CID
  -> m (Either IPFS.Error.Add Integer)
getSize (CID hash) = IPFS.Proc.run ["object", "stat"] (Lazy.fromStrict <| encodeUtf8 hash) >>= \case
  (ExitSuccess, result, _) -> do
    case parseSize result of
      Nothing -> return . Left . UnexpectedOutput <| "Could not parse CumulativeSize"
      Just (size, _) -> return <| Right size

  (ExitFailure _ , _, err) -> return . Left . UnknownAddErr <| UTF8.textShow err

parseSize :: Lazy.ByteString -> Maybe (Integer, Lazy.ByteString)
parseSize = CL.readInteger . List.last . CL.words . List.last . CL.lines

createLink ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => IPFS.CID
  -> IPFS.Name
  -> m (Either IPFS.Error.Add Link)
createLink cid name = getSize cid >>= \case
  Left err -> return <| Left err
  Right size -> return . Right <| Link
    { cid = cid
    , name = name
    , size = size
    }
