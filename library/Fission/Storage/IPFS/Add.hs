module Fission.Storage.IPFS.Add (walk, getSize) where

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List as List

import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Directory

import           Fission.Prelude
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Node.Types as DAG
import           Fission.IPFS.DAG.Link.Types as DAG
import qualified Fission.Storage.IPFS    as IPFS


walk ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => FilePath
  -> m (Either IPFS.Error.Add DAG.Node)
walk path = do
  files <- listDirectory path
  let names = IPFS.Name <$> files
  results <- mapM IPFS.addDir files
  case lefts results of
    [] -> return . Left . List.head <| lefts results

    _  -> do
    links <- zipWithM createLink (rights results) names
    case lefts links of
      [] -> return . Left . List.head <| lefts links
      _  -> return <| Right Node
        { dataBlock = "CAE="
        , links = rights links
        }

getSize ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => IPFS.CID
  -> m (Either IPFS.Error.Add Int)
getSize (CID hash) = IPFS.Proc.run ["object", "stat"] (Lazy.fromStrict <| encodeUtf8 hash) >>= \case
  (ExitSuccess, result, _) -> do
    case parseSize result of
      Nothing -> return . Left . UnexpectedOutput <| "Could not parse CumulativeSize"
      Just (size, _) -> return <| Right size

  (ExitFailure _ , _, err) -> 
    return . Left . UnknownAddErr <| UTF8.textShow err

parseSize :: Lazy.ByteString -> Maybe (Int, Lazy.ByteString)
parseSize = CL.readInt . List.last . CL.words . List.last . CL.lines
-- "QmYcWzXNBc8WK1PUKeyPBRBJWo3tGhso2EPgKdEjbduaF1"

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
