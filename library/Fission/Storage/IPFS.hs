module Fission.Storage.IPFS
  ( addDir
  , addRaw
  , addFile
  , get
  , getContent
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Data.Has
import Data.ByteString.Lazy.Char8 as CL

import qualified Network.HTTP.Client as HTTP

import           Fission.Internal.Constraint
import           Fission.Internal.Process
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.Config              as Config
import qualified Fission.File.Types          as File
import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import qualified Fission.Storage.IPFS.Pin    as IPFS.Pin

addRaw :: MonadRIO          cfg m
       => HasProcessContext cfg
       => HasLogFunc        cfg
       => Has HTTP.Manager  cfg
       => Has IPFS.URL      cfg
       => Has IPFS.BinPath  cfg
       => Has IPFS.Timeout  cfg
       => Lazy.ByteString
       -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw =
  IPFS.Proc.run ["add", "-HQ"] raw >>= \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> IPFS.Pin.add . mkCID . UTF8.stripN 1 $ UTF8.textShow cid
        bad   -> return . Left . UnexpectedOutput $ UTF8.textShow bad

    (ExitFailure _, _, err) ->
      return . Left . UnknownAddErr $ UTF8.textShow err

addFile :: MonadRIO          cfg m
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => Has HTTP.Manager  cfg
        => Has IPFS.URL      cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => Lazy.ByteString
        -> IPFS.Name
        -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name =
  IPFS.Proc.run opts raw >>= \case
    (ExitSuccess, result, _) ->
      IPFS.Pin.add (mkCID $ UTF8.textShow result) >>= \case
        Left err ->
          return . Left . UnknownAddErr $ UTF8.textShow err

        Right _ ->
          case CL.lines result of
            [inner, outer] ->
              let
                sparseTree  = Directory [(Hash rootCID, fileWrapper)]
                fileWrapper = Directory [(fileName, Content fileCID)]
                rootCID     = CID $ UTF8.textShow outer
                fileCID     = CID . UTF8.stripN 1 $ UTF8.textShow inner
                fileName    = Key name
              in
                return $ Right sparseTree

            bad ->
              return . Left . UnexpectedOutput $ UTF8.textShow bad


    (ExitFailure _, _, err) ->
      return . Left . UnknownAddErr $ UTF8.textShow err

    where
      opts = [ "add"
             , "-wq"
             , "--stdin-name"
             , unName name
             ]

addDir :: RIOProc           cfg m
       => Has IPFS.Timeout  cfg
       => Has IPFS.BinPath  cfg
       => FilePath
       -> m (Either IPFS.Error.Add CID)
addDir path = IPFS.Proc.run ["add", "-HQr", path] "" >>= pure . \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> Right . mkCID . UTF8.stripN 1 $ UTF8.textShow cid
        bad   -> Left . UnexpectedOutput $ UTF8.textShow bad

    (ExitFailure _, _, err) ->
      Left . UnknownAddErr $ UTF8.textShow err

-- TODO rename to get and cat or something similar. getSingle?
getContent :: RIOProc           cfg m
      => Has IPFS.Timeout  cfg
      => Has IPFS.BinPath  cfg
      => IPFS.CID
      -> m (Either IPFS.Error.Get CL.ByteString)
getContent cid@(IPFS.CID hash) = IPFS.Proc.run ["get", Text.unpack hash] "" >>= \case
  (ExitSuccess, contents, _) ->
    return . Right $ contents

  (ExitFailure _, _, stdErr) ->
    return . Left . UnknownGetErr $ UTF8.textShow stdErr

get :: RIOProc           cfg m
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => IPFS.CID
    -> m (Either IPFS.Error.Get File.Serialized)
get cid@(IPFS.CID hash) = IPFS.Proc.run ["cat"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, contents, _) ->
    return . Right $ File.Serialized contents

  (ExitFailure _, _, stdErr)
    | Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" stdErr ->
        return . Left $ InvalidCID hash

    | Lazy.isSuffixOf "context deadline exceeded" stdErr -> do
        Timeout seconds <- Config.get
        return . Left $ TimedOut cid seconds

    | otherwise ->
        return . Left . UnknownGetErr $ UTF8.textShow stdErr
