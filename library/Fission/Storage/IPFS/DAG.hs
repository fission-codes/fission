module Fission.Storage.IPFS.DAG
  ( put ) where

import           Data.ByteString.Lazy.Char8 as CL
import qualified Network.HTTP.Client as HTTP
import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.Storage.IPFS.Pin    as IPFS.Pin

put :: MonadRIO cfg m
        => HasProcessContext cfg
        => HasLogFunc cfg
        => Has HTTP.Manager  cfg
        => Has IPFS.URL      cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => Lazy.ByteString
        -> m (Either IPFS.Error.Add IPFS.CID)
put raw = IPFS.Proc.run ["dag", "put", "-f", "dag-pb"] raw >>= \case
  (ExitSuccess, result, _) ->
    case CL.lines result of
      [cid] ->
        cid
          |> UTF8.textShow
          |> UTF8.stripN 1
          |> mkCID
          |> IPFS.Pin.add

      bad ->
        pure . Left . UnexpectedOutput <| UTF8.textShow bad

  (ExitFailure _, _, err) ->
    pure . Left . UnknownAddErr <| UTF8.textShow err
