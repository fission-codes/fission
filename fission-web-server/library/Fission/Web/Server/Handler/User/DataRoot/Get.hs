module Fission.Web.Server.Handler.User.DataRoot.Get (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.DataRoot.Get.Types as API.User

import qualified Fission.Web.Server.Error                as Web.Err
import           Fission.Web.Server.WNFS                 as WNFS

handler :: (MonadLogger m, MonadThrow m, MonadWNFS m) => ServerT API.User.GetRoot m
handler username = Web.Err.ensureM $ WNFS.getUserDataRoot username
