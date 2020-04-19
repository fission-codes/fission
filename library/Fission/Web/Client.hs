module Fission.Web.Client
  ( sendRequestM
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import           Servant.Client

import           Fission.Prelude
 
import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class

sendRequestM :: MonadWebClient m => m (ClientM a) -> m (Either ClientError a)
sendRequestM clientAction = sendRequest =<< clientAction
