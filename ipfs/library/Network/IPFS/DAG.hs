module Network.IPFS.DAG
  ( put
  , putNode
  , putRemote
  ) where

import           Data.ByteString.Lazy.Char8        as CL
import qualified Network.IPFS.Internal.UTF8        as UTF8
import qualified RIO.ByteString.Lazy               as Lazy

import           Servant.Client
import qualified Servant.Multipart.Client          as Multipart.Client

import           Network.IPFS.Prelude

import           Network.IPFS.Add.Error            as IPFS.Add
import qualified Network.IPFS.Client               as IPFS.Client
import           Network.IPFS.Client.DAG.Put.Types as DAG.Put
import           Network.IPFS.DAG.Node.Types       as DAG
import           Network.IPFS.File.Form.Types      as File
import           Network.IPFS.File.Types           as File
import           Network.IPFS.Local.Class          as IPFS
import           Network.IPFS.Remote.Class         as IPFS
import           Network.IPFS.Types                as IPFS

put :: MonadLocalIPFS m => Lazy.ByteString -> m (Either IPFS.Add.Error IPFS.CID)
put raw = IPFS.runLocal ["dag", "put", "-f", "dag-pb"] raw >>= \case
  Right result ->
    case CL.lines result of
      [cid] ->
        cid
          |> UTF8.textShow
          |> UTF8.stripN 1
          |> mkCID
          |> Right
          |> return

      bad ->
        pure . Left . UnexpectedOutput $ UTF8.textShow bad

  Left err ->
    pure . Left . UnknownAddErr $ UTF8.textShow err

putNode :: MonadLocalIPFS m => DAG.Node -> m (Either IPFS.Add.Error IPFS.CID)
putNode node = put $ encode node

putRemote :: MonadRemoteIPFS m => File.Serialized -> m (Either ClientError DAG.Put.Response)
putRemote file = do
  boundary  <- liftIO Multipart.Client.genBoundary
  runRemote (IPFS.Client.dagPut True (boundary, File.Form "file" file))
