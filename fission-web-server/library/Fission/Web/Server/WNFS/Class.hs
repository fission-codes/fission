module Fission.Web.Server.WNFS.Class (MonadWNFS(..)) where

import           Fission.Prelude
import           Servant

import           Fission.User.Username.Types
import           Network.IPFS.CID.Types

class Monad m => MonadWNFS m where
  getUserDataRoot :: Username -> m (Either ServerError CID)
