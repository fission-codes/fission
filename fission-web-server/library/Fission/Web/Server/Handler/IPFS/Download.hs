module Fission.Web.Server.Handler.IPFS.Download (get) where

import           Network.IPFS.Remote.Class           as IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Download.Types as API.IPFS

import qualified Fission.Web.Server.Error            as Web.Err

get :: (MonadRemoteIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.Download m
get = pathGet :<|> queryGet

queryGet :: (MonadRemoteIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.ViaQuery m
queryGet = \case
  Just cid -> Web.Err.ensureM $ IPFS.ipfsCat cid
  Nothing  -> throwM err404 { errBody = "Missing CID" }

pathGet :: (MonadRemoteIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.ViaPath m
pathGet cid = Web.Err.ensureM $ IPFS.ipfsCat cid
