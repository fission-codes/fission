module Fission.Storage.IPFS.DAG
  ( put
  , putNode
  ) where

import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFS.Proc
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Node.Types as DAG

put :: 
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
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
          |> Right
          |> return

      bad ->
        pure . Left . UnexpectedOutput <| UTF8.textShow bad

  (ExitFailure _, _, err) ->
    pure . Left . UnknownAddErr <| UTF8.textShow err

putNode :: 
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => DAG.Node
  -> m (Either IPFS.Error.Add IPFS.CID)
putNode node = put <| encode node
