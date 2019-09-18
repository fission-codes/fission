module Fission.Storage.IPFS.Add
  ( addRaw
  , addFile
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has
import Data.ByteString.Lazy.Char8 as CL

import qualified Network.HTTP.Client as HTTP

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.Storage.IPFS.Pin    as Pin

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
  IPFS.Proc.run ["add", "-q"] raw >>= \case
    (ExitSuccess, result, _) ->
      case CL.lines result of
        [cid] -> pin . mkCID . UTF8.stripN 1 $ UTF8.textShow cid
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
    (ExitSuccess, result, _) -> do
      pin (mkCID $ UTF8.textShow result) >>= \case
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