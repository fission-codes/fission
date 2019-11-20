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

import           Fission.Prelude hiding (link)
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
  => IPFS.Ignored
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
  => IPFS.Ignored
  -> FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
walkDir ignored path = do
  files <- listDirectory path
  let toAdd = removeIgnored ignored files
  let f = foldResults path ignored
  foldM f (Right <| Node { dataBlock = "CAE=", links = [] }) toAdd >>= \case
    Left err -> return <| Left err
    Right node -> DAG.putNode node

foldResults ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => FilePath
  -> IPFS.Ignored
  -> Either IPFS.Error.Add Node
  -> FilePath
  -> m (Either IPFS.Error.Add Node)
foldResults _ _ (Left err) _ = return <| Left err
foldResults path ignored (Right node) filename = do
  addDir ignored (path </> filename) >>= \case
    Left err ->  return <| Left err
    Right cid ->
      DAG.Link.create cid (IPFS.Name filename) >>= \case
      Left err -> return <| Left err
      Right link ->
        return <| Right <| node { links = link:(links node) }

removeIgnored :: IPFS.Ignored -> [FilePath] -> [FilePath]
removeIgnored ignored files = List.filter (not . matchesAny ignored) files

matchesAny :: IPFS.Ignored -> FilePath -> Bool
matchesAny globs path = List.any (\x -> Glob.match x path) globs
