module Fission.Web.Server.Handler.User.DataRoot.Get (handler) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude

import           Fission.User.Username.Types

import qualified Fission.Web.Server.Error    as Web.Err
import           Fission.Web.Server.WNFS     as WNFS

handler :: ( MonadLogger m, MonadThrow m, MonadWNFS m) => ServerT API m
handler username = Web.Err.ensureM $ WNFS.getUserDataRoot username
