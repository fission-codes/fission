module Fission.Web.Server.Handler.IPFS.Download (get) where

import           Network.IPFS
import qualified Network.IPFS.Get                    as IPFS

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Download.Types as API.IPFS

import qualified Fission.Web.Server.Error            as Web.Err

get :: (MonadLocalIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.Download m
get = pathGet :<|> queryGet

queryGet :: (MonadLocalIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.ViaQuery m
queryGet = \case
  Just cid -> IPFS.getFile cid >>= Web.Err.ensure
  Nothing  -> throwM err404

pathGet :: (MonadLocalIPFS m, MonadLogger m, MonadThrow m) => ServerT API.IPFS.ViaPath m
pathGet cid = IPFS.getFile cid >>= Web.Err.ensure
