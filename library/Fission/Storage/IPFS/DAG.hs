module Fission.Storage.IPFS.DAG
  ( put ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import           SuperRecord
import           Data.ByteString.Lazy.Char8 as CL

import qualified Network.HTTP.Client as HTTP
import qualified Servant.Client      as Client

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.Storage.IPFS.Pin    as IPFS.Pin

put :: MonadRIO (Rec cfg) m
    => HasProcessContext (Rec cfg)
    => HasLogFunc (Rec cfg)
    => Has "httpManager" cfg HTTP.Manager
    => Has "ipfsURL"     cfg Client.BaseUrl
    => Has "ipfsPath"    cfg IPFS.BinPath
    => Has "ipfsTimeout" cfg IPFS.Timeout
    => Lazy.ByteString
    -> m (Either IPFS.Error.Add IPFS.CID)
put raw = IPFS.Proc.run ["dag", "put", "-f", "dag-pb"] raw >>= \case
  (ExitSuccess, result, _) ->
    case CL.lines result of
      [cid] ->  IPFS.Pin.add . mkCID . UTF8.stripN 1 $ UTF8.textShow cid
      bad   -> pure . Left . UnexpectedOutput $ UTF8.textShow bad

  (ExitFailure _, _, err) ->
    pure . Left . UnknownAddErr $ UTF8.textShow err
