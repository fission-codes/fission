module Fission.Storage.IPFS
  ( addRaw
  , addFile
  , get
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import           SuperRecord hiding (get)
import           Data.ByteString.Lazy.Char8 as CL

import qualified Network.HTTP.Client as HTTP
import qualified Servant.Client      as Client

import           Fission.Internal.Constraint
import           Fission.Internal.Process
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.File.Types          as File
import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import qualified Fission.Storage.IPFS.Pin    as IPFS.Pin

addRaw :: MonadRIO          (Rec cfg) m
       => HasProcessContext (Rec cfg)
       => HasLogFunc        (Rec cfg)
       => HasOf [ "httpManager" := HTTP.Manager
               , "ipfsURL"     := Client.BaseUrl
               , "ipfsPath"    := FilePath
               , "ipfsTimeout" := Natural
               ]  cfg
       => Lazy.ByteString
       -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw =
  IPFS.Proc.run ["add", "-q"] raw >>= \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> IPFS.Pin.add . mkCID . UTF8.stripN 1 $ UTF8.textShow cid
        bad   -> return . Left . UnexpectedOutput $ UTF8.textShow bad

    (ExitFailure _, _, err) ->
      return . Left . UnknownAddErr $ UTF8.textShow err

addFile :: MonadRIO          (Rec cfg) m
        => HasProcessContext (Rec cfg)
        => HasLogFunc        (Rec cfg)
        => Has "httpManager" cfg HTTP.Manager
        => Has "ipfsURL"     cfg Client.BaseUrl
        => Has "ipfsPath"    cfg FilePath
        => Has "ipfsTimeout" cfg Natural
        => Lazy.ByteString
        -> IPFS.Name
        -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name =
  IPFS.Proc.run opts raw >>= \case
    (ExitSuccess, result, _) -> do
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

get :: RIOProc           (Rec cfg) m
    => Has "ipfsPath"    cfg FilePath
    => Has "ipfsTimeout" cfg Natural
    => IPFS.CID
    -> m (Either IPFS.Error.Get File.Serialized)
get cid@(IPFS.CID hash) = IPFS.Proc.run ["cat"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, contents, _) ->
    return . Right $ File.Serialized contents

  (ExitFailure _, _, stdErr)
    | Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" stdErr ->
        return . Left $ InvalidCID hash

    | Lazy.isSuffixOf "context deadline exceeded" stdErr -> do
        seconds <- asksR #ipfsTimeout
        return . Left $ TimedOut cid seconds

    | otherwise ->
        return . Left . UnknownGetErr $ UTF8.textShow stdErr
