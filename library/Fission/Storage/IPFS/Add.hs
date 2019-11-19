module Fission.Storage.IPFS.Add
  ( addRaw
  , addFile
  , addPath
  , addDir
  ) where

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List                  as List

import qualified Network.HTTP.Client  as HTTP
import qualified System.FilePath.Glob as Glob

import           RIO.Directory
import           RIO.FilePath
import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import           Fission.Internal.Process
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Node.Types as DAG
import           Fission.IPFS.DAG.Link       as DAG.Link

import           Fission.Storage.IPFS.DAG as DAG
import qualified Fission.Storage.IPFS.Pin as IPFS.Pin

addRaw ::
  ( MonadRIO          cfg m
  , HasProcessContext cfg
  , HasLogFunc        cfg
  , Has HTTP.Manager  cfg
  , Has IPFS.URL      cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => Lazy.ByteString
  -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw =
  IPFS.Proc.run ["add", "-HQ"] raw >>= \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] ->
          cid
            |> UTF8.textShow
            |> UTF8.stripN 1
            |> mkCID
            |> IPFS.Pin.add

        bad ->
          return . Left . UnexpectedOutput <| UTF8.textShow bad

    (ExitFailure _, _, err) ->
      return . Left . UnknownAddErr <| UTF8.textShow err

addFile ::
  ( MonadRIO          cfg m
  , HasProcessContext cfg
  , HasLogFunc        cfg
  , Has HTTP.Manager  cfg
  , Has IPFS.URL      cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => Lazy.ByteString
  -> IPFS.Name
  -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name =
  IPFS.Proc.run opts raw >>= \case
    (ExitSuccess, result, _) ->
      IPFS.Pin.add (mkCID <| UTF8.textShow result) >>= \case
        Left err ->
          return . Left . UnknownAddErr <| UTF8.textShow err

        Right _ ->
          case CL.lines result of
            [inner, outer] ->
              let
                sparseTree  = Directory [(Hash rootCID, fileWrapper)]
                fileWrapper = Directory [(fileName, Content fileCID)]
                rootCID     = CID <| UTF8.textShow outer
                fileCID     = CID . UTF8.stripN 1 <| UTF8.textShow inner
                fileName    = Key name
              in
                return <| Right sparseTree

            bad ->
              return . Left . UnexpectedOutput <| UTF8.textShow bad


    (ExitFailure _, _, err) ->
      return . Left . UnknownAddErr <| UTF8.textShow err

    where
      opts = [ "add"
             , "-wq"
             , "--stdin-name"
             , unName name
             ]

addPath ::
  ( RIOProc           cfg m
  , Has IPFS.Timeout  cfg
  , Has IPFS.BinPath  cfg
  )
  => FilePath
  -> m (Either IPFS.Error.Add CID)
addPath path = IPFS.Proc.run ["add", "-HQ", path] "" >>= pure . \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> Right . mkCID . UTF8.stripN 1 <| UTF8.textShow cid
        bad   -> Left . UnexpectedOutput <| UTF8.textShow bad

    (ExitFailure _, _, err) ->
      Left . UnknownAddErr <| UTF8.textShow err

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
  True -> addPath path
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
removeIgnored ignored files = List.filter (not . matchesAny ignored) files

matchesAny :: [Glob.Pattern] -> FilePath -> Bool
matchesAny globs path = List.any (\x -> Glob.match x path) globs
              
firstErr :: [Either a b] -> a
firstErr = List.head . lefts
