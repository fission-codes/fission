module Web.UCAN.Resolver.Error (Error (..)) where

import           Network.IPFS.CID.Types
import           RIO
import           Servant.Client.Core
import           Web.UCAN.Internal.Orphanage.ClientError ()

data Error
  = CannotResolve CID ClientError
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    CannotResolve cid err ->
      "Unable to resolve " <> display cid <> " because " <> display err
