module Fission.Storage.IPFS
  ( addRaw
  , addFile
  , get
  , pin
  , unpin
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has
import Data.ByteString.Lazy.Char8 as CL

import qualified Fission.Config              as Config
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.Internal.Constraint
import           Fission.Internal.Process
import qualified Fission.File.Types          as File
import qualified Fission.IPFS.Process        as IPFS.Proc
import qualified Fission.Internal.UTF8       as UTF8

get :: RIOProc           cfg m
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => IPFS.CID
    -> m (Either IPFS.Error.Get File.Serialized)
get cid@(IPFS.CID hash) = IPFS.Proc.run ["cat"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, contents, _) ->
    return . Right $ File.Serialized contents

  (ExitFailure _, _, Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" -> True) ->
    return . Left $ InvalidCID hash

  (ExitFailure _, _, Lazy.isSuffixOf "context deadline exceeded" -> True) -> do
    Timeout seconds <- Config.get
    return . Left $ TimedOut cid seconds

  (ExitFailure _, _, errStr) ->
    return . Left . UnknownGetErr $ UTF8.textShow errStr

addRaw :: MonadRIO          cfg m
       => HasProcessContext cfg
       => HasLogFunc        cfg
       => Has IPFS.BinPath  cfg
       => Has IPFS.Timeout  cfg
       => Lazy.ByteString
       -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw =
  IPFS.Proc.run ["add", "-q"] raw <&> \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> Right . mkCID $ UTF8.textShow cid
        bad   -> Left . UnexpectedOutput $ UTF8.textShow bad

    (ExitFailure _, _, err) ->
      Left . UnknownAddErr $ UTF8.textShow err

addFile :: MonadRIO          cfg m
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => Lazy.ByteString
        -> IPFS.Name
        -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name =
  IPFS.Proc.run opts raw <&> \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [inner, outer] ->
          let
            sparseTree  = Directory [(Hash rootCID, fileWrapper)]
            fileWrapper = Directory [(fileName, Content fileCID)]
            rootCID     = CID $ UTF8.textShow outer
            fileCID     = CID $ UTF8.textShow inner
            fileName    = Key name
          in
            Right sparseTree

        bad ->
          Left . UnexpectedOutput $ UTF8.textShow bad

    (ExitFailure _, _, err) ->
      Left . UnknownAddErr $ UTF8.textShow err

    where
      opts = [ "add"
            , "-wq"
            , "--stdin-name"
            , unName name
            ]

pin :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => IPFS.CID
    -> m (Either IPFS.Error.Add CID)
pin cid@(CID hash) = IPFS.Proc.runErr' ["pin", "add"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, _) -> do
    logDebug $ "Pinned CID " <> display hash
    return $ Right cid

  (ExitFailure _, errStr) -> do
    let err = UnknownAddErr $ UTF8.textShow errStr
    logError $ display err
    return $ Left err

-- | Unpin a CID
unpin :: MonadRIO          cfg m
      => HasProcessContext cfg
      => HasLogFunc        cfg
      => Has IPFS.BinPath  cfg
      => Has IPFS.Timeout  cfg
      => IPFS.CID
      -> m (Either IPFS.Error.Add CID)
unpin cid@(CID hash) = IPFS.Proc.runErr' ["pin", "rm"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, _) -> do
    logDebug $ "Unpinned CID " <> display hash
    return $ Right cid

  (ExitFailure _, Lazy.take 17 -> "Error: not pinned") -> do
    logDebug $ "Cannot unpin CID " <> display hash <> " because it was not pinned"
    return $ Right cid

  (ExitFailure _, errStr) -> do
    let err = UnknownAddErr $ UTF8.textShow errStr
    logError $ display err
    return $ Left err
