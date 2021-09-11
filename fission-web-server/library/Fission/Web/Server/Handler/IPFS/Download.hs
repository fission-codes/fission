module Fission.Web.Server.Handler.IPFS.Download (handler) where

import           Network.IPFS.Remote.Class           as IPFS

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Download.Types as IPFS.Download

import qualified Fission.Web.Server.Error            as Web.Err

handler :: (MonadRemoteIPFS m, MonadLogger m, MonadThrow m) => IPFS.Download.Routes (AsServerT m)
handler = IPFS.Download.Routes {..}
  where
    viaPath cid =
      Web.Err.ensureM $ IPFS.ipfsCat cid

    viaQuery = \case
      Just cid -> Web.Err.ensureM $ IPFS.ipfsCat cid
      Nothing  -> throwM err404 { errBody = "Missing CID" }

