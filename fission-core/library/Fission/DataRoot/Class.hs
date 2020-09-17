module Fission.DataRoot.Class
  ( MonadDataRoot(..)
  )
where

import           Fission.Prelude
import           Servant

import           Fission.AWS.Route53
import           Fission.User.Username.Types
import           Network.IPFS.CID.Types

class MonadRoute53 m => MonadDataRoot m where
  get :: Username -> m (Either ServerError CID)
