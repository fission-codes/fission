module Fission.Storage.IPFS
  ( addRaw
  , addFile
  , pin
  , unpin
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has
import Data.ByteString.Lazy.Char8 as CL

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import qualified Fission.IPFS.Process        as IPFS.Proc

addRaw :: MonadRIO          cfg m
       => HasProcessContext cfg
       => HasLogFunc        cfg
       => Has IPFS.BinPath  cfg
       => Lazy.ByteString
       -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw = IPFS.Proc.run ["add", "-q"] raw <&> CL.lines <&> \case
  [cid] -> Right . mkCID $ UTF8.textShow cid
  bad   -> Left . UnexpectedOutput $ UTF8.textShow bad

addFile :: MonadRIO          cfg m
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => Has IPFS.BinPath  cfg
        => Lazy.ByteString
        -> IPFS.Name
        -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name = IPFS.Proc.run opts raw <&> CL.lines <&> \case
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
    => IPFS.CID
    -> m (Either IPFS.Error.Add CID)
pin cid@(CID hash) = IPFS.Proc.run_ ["pin", "add"] (UTF8.textToLazyBS hash) <&> \case
  ExitSuccess   -> Right cid
  ExitFailure _ -> Left UnknownError

-- | Unpin a CID
unpin :: MonadRIO          cfg m
      => HasProcessContext cfg
      => HasLogFunc        cfg
      => Has IPFS.BinPath  cfg
      => IPFS.CID
      -> m (Either IPFS.Error.Add CID)
unpin cid@(CID hash) = IPFS.Proc.runErr' ["pin", "rm"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, _) -> do
    logDebug $ "Unpinned CID " <> display hash
    return $ Right cid

  (ExitFailure _, Lazy.take 17 -> "Error: not pinned") -> do
    logDebug $ "Cannot unpin CID " <> display hash <> " because it was not pinned"
    return $ Right cid

  _ ->
    return $ Left UnknownError
