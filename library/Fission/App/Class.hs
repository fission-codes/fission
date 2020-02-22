module Fission.AppDomain.Class where

import Network.IPFS.CID.Types

import Fission.Prelude
import Fission.Models

class Monad m => MonadApp m where
  create :: UserId -> CID -> UTCTime -> m (Maybe AppId)
